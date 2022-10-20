       ID DIVISION.                                                     00000010
       PROGRAM-ID.       PRNTFICH.                                      00000020
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * * * * * *   P R O G R A M   C H A N G E   L O G   * * * * * * *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * CR#  OR  IR#    DATE     PGMR       CHANGE DESCRIPTION        *
      * ------------- ---------- ---- ------------------------------- *
      * 1998031800003 04/14/1999 DJWB COBOL II-CHGD POSITIONING TO    *
      *                                ADVANCING-CHGD CARRIAGE CNTL   *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       ENVIRONMENT DIVISION.                                            00000030
       INPUT-OUTPUT SECTION.                                            00000040
       FILE-CONTROL.                                                    00000050
           SELECT  FICH-IN    ASSIGN TO SYS020.                         00000060
           SELECT  PRNT-OUT   ASSIGN TO SYS008.                         00000070
       DATA DIVISION.                                                   00000080
       FILE SECTION.                                                    00000090
       FD  FICH-IN                                                      00000100
           RECORDING MODE IS F                                          00000110
           LABEL RECORDS ARE STANDARD                                   00000120
           BLOCK CONTAINS 0 RECORDS                                     00000140
           DATA RECORD IS FICH-REC.                                     00000150
      *01  FICH-REC                        PIC  X(133).                 00000160
       01  FICHE-RECORD.
           05  F-CTL                       PIC  X(1).                   00000160
           05  FICH-REC                    PIC  X(132).                 00000160
                                                                        00000170
       FD  PRNT-OUT                                                     00000180
           RECORDING MODE IS F                                          00000190
           LABEL RECORDS ARE OMITTED                                    00000200
           DATA RECORD IS PRT.                                          00000220
       01  PRT.                                                         00000230
      *    12  P-CTL               PIC  X(01).                          00000240
           12  P-DATA              PIC  X(132).                         00000250
       WORKING-STORAGE SECTION.                                         00000260
      *77  X             PIC X     VALUE SPACE.                         00000270
       PROCEDURE DIVISION.                                              00000280
           OPEN INPUT  FICH-IN                                          00000290
                OUTPUT PRNT-OUT.                                        00000300
       READ-LOOP.                                                       00000310
           READ FICH-IN                                                 00000320
               AT END GO TO END-OF-JOB.                                 00000330
           MOVE FICH-REC TO PRT.                                        00000340
      *    MOVE P-CTL TO X.                                             00000350
           EVALUATE F-CTL
              WHEN ' '
                 WRITE PRT AFTER ADVANCING 1 LINE
              WHEN '0'
                 WRITE PRT AFTER ADVANCING 2 LINE
              WHEN '-'
                 WRITE PRT AFTER ADVANCING 3 LINE
              WHEN '+'
                 WRITE PRT AFTER ADVANCING 0 LINE
              WHEN '1'
                 WRITE PRT AFTER ADVANCING PAGE
              WHEN OTHER
                 DISPLAY 'INVALID CARRIAGE CONTROL: ' F-CTL
           END-EVALUATE.
      *    WRITE PRT AFTER ADVANCING X.                                 00000360
           GO TO READ-LOOP.                                             00000370
       END-OF-JOB.                                                      00000380
           CLOSE FICH-IN WITH LOCK,  PRNT-OUT.                          00000390
           goback
           .
