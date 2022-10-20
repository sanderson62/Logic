       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.  FIXCIB0.                                            00020000
                                                                        00030000
      ******************************************************************00040000
      *                     PROGRAM DESCRIPTION                        *00050000
      ******************************************************************00060000
      * THIS PROGRAM READS THE MICR DRAFT FILE AND SELECTS ALL CID     *00070000
      * CLAIM DRAFT RECORDS.  FOR EACH RECORD SELECTED, TWO OUTPUT     *00080000
      * RECORDS ARE  CREATED:                                          *00090000
      *                                                                *00100000
      *   1)  A DATA RECORD TO PRINT THE DRAFT COPY                    *00110000
      *   2)  A DATA RECORD TO PRINT THE CLAIM DRAFT COVER LETTER      *00120000
      *                                                                *00130000
      ******************************************************************00140000
      *                       PROGRAM HISTORY                          *00150000
      ******************************************************************00160000
      *                                                                *00170000
      *  DANA  NEW CR#1998020500005                                    *00180000
      ******************************************************************00190000
                                                                        00200000
                                                                        00210000
       ENVIRONMENT DIVISION.                                            00220000
       INPUT-OUTPUT SECTION.                                            00230000
       FILE-CONTROL.                                                    00240000
                                                                        00250000
           SELECT MICR-DRAFT-FILE                                       00260000
               ASSIGN TO MICRDRFT                                       00270000
               ORGANIZATION IS INDEXED                                  00280000
               ACCESS IS SEQUENTIAL                                     00290000
               RECORD KEY IS MICR-KEY                                   00300000
               FILE STATUS IS MICR-STATUS.                              00310000
                                                                        00320000
           SELECT DRAFT-COPIES                                          00330000
               ASSIGN TO SYS007.                                        00340000
                                                                        00350000
           SELECT COVER-LETTERS                                         00360005
               ASSIGN TO SYS008.                                        00370000
                                                                        00410000
           SELECT comment-cards
                  organization is line sequential
               ASSIGN TO SYS009.                                        00370000
                                                                        00420000
       DATA DIVISION.                                                   00430000
       FILE SECTION.                                                    00440000
                                                                        00450000
       FD  MICR-DRAFT-FILE                                              00460000
           RECORD IS VARYING FROM 363 TO 3000 CHARACTERS.               00470000
       01  FILLER.                                                      00480000
           05  MICR-KEY      PIC X(0019).                               00490000
           05  FILLER        PIC X(2981).                               00500000
       01  MICR-RECORD.                                                 00510000
           COPY MICR420C.                                               00520000
                                                                        00530000
       FD  DRAFT-COPIES                                                 00540000
           LABEL RECORDS ARE STANDARD                                   00550000
           RECORDING MODE IS F                                          00560000
           BLOCK CONTAINS 0 RECORDS.                                    00570000
       01  DRAFT-COPY       PIC X(1341).                                00580000
                                                                        00590000
       FD  COVER-LETTERS                                                00600005
           LABEL RECORDS ARE STANDARD                                   00610000
           RECORDING MODE IS F                                          00620000
           BLOCK CONTAINS 0 RECORDS.                                    00630000
       01  COVER-LETTER     PIC X(384).                                 00640005

       FD  comment-cards                                                00600005
           LABEL RECORDS ARE STANDARD                                   00610000
           RECORDING MODE IS F                                          00620000
           BLOCK CONTAINS 0 RECORDS.                                    00630000
       01  comment-card-rec PIC X(44).                                  00640005
                                                                        00650000
                                                                        00650000
                                                                        00720000
       EJECT                                                            00730000
                                                                        00740000
       WORKING-STORAGE SECTION.                                         00750000
       copy "ctypes.cpy".
                                                                        00760000
       01  FILLER.                                                      00770000
           05  SUB          PIC S9(4) BINARY.                           00780000
           05  S0C7         PIC X   VALUE SPACE.                        00790000
           05  FORCE-DUMP   REDEFINES S0C7 PIC S9 COMP-3.               00800000
           05  MICR-STATUS  PIC XX  VALUE '00'.                         00810000
               88  EOF              VALUE '10'.                         00820000
           05  WS-WORK      PIC X(30)  VALUE SPACE.                     00830001
           05  SET-PERIOD-SW        PIC XXX  VALUE SPACES.              00840001
               88  PERIOD-IS-SET    VALUE IS 'YES'.                     00850001
           05  WORK-TABLE.                                              00860000
               10  WORK-ADDR OCCURS 5 TIMES PIC X(50).                  00870000
                                                                        00880000
       01  DRAFT-RECORD.                                                00890000
           05  DR-SEQ              PIC ZZZ9.                            00900000
           05  DR-AMT1             PIC ZZZ,ZZZ,ZZZ.99.                  00910000
           05  DR-AMT2             PIC ***,***,***.99.                  00920000
           05  DR-MICR-REC         PIC X(1181).                         00930000
           05  DR-BARCODE          PIC X(128).                          00940000
                                                                        00950000
       01  comment-record.                                              00890000
           05  cr-file-name        pic x(10) value 'survey.doc'.
           05  cr-tab1             PIC x.                               00910000
           05  cr-claim-no         PIC x(7).                            00900000
           05  cr-tab2             PIC x.                               00910000
           05  cr-barcode          PIC X(25).                           00940000
                                                                        00950000
       01  LETTER-RECORD.                                               00960000
           05  LR-LETTER-ID        PIC X(4).                            00970005
           05  LR-PAYEE-NAME       PIC X(30).                           00971005
           05  LR-PAYEE-ADDR1      PIC X(40).                           00980000
           05  LR-PAYEE-ADDR2      PIC X(40).                           00990000
           05  LR-PAYEE-ADDR3      PIC X(40).                           01000000
           05  LR-PAYEE-ADDR4      PIC X(40).                           01010000
           05  LR-MEMBER-NAME      PIC X(30).                           01020000
           05  LR-LOAN-NO          PIC X(25).                           01030000
           05  LR-CLAIM-NO         PIC X(7).                            01040000
           05  LR-BARCODE          PIC X(128).                          01050000
                                                                        01060000
       01  CID-BARCODE.                                                 01070000
           05  BC-ENCL-CODE        PIC XX       VALUE ZERO.             01080000
           05  BC-MAIL-CODE        PIC X        VALUE '1'.              01090000
           05  BC-DIV-CODE         PIC X        VALUE '1'.              01100000
           05  BC-CLAIM-NO         PIC X(7)     VALUE SPACE.            01110000
           05  BC-CERT-NO          PIC X(10)    VALUE SPACE.            01120004
           05  BC-SEQ              PIC 9(4)     VALUE ZERO.             01130000
                                                                        01140000
      ***  AGEB16 IS USED TO BUILD THE BARCODE.  IT CALLS EANSRC ***    01150000
       01  AGEB16-PARMS.                                                01160000
      *     05  AGEB16-BC-LEN       PIC 9(4).                           01170000
           05  AGEB16-BC-LEN       short.
           05  AGEB16-BC-IN        PIC X(28).                           01180000
           05  AGEB16-BC-OUT       PIC X(128).                          01190000
                                                                        01200000
                                                                        01210000
           EJECT                                                        01220000
      *                                                                 01230000
       PROCEDURE DIVISION.                                              01240000
      *                                                                 01250000
           OPEN INPUT MICR-DRAFT-FILE                                   01260000
           IF MICR-STATUS = '00' OR '97'                                01270000
              CONTINUE                                                  01280000
           ELSE                                                         01290000
              DISPLAY 'OPEN ERROR ' MICR-STATUS ' ON MICRDRFT FILE'     01300000
              ADD +1 TO FORCE-DUMP.                                     01310000
                                                                        01320000
           OPEN OUTPUT DRAFT-COPIES
                       comment-cards
                       COVER-LETTERS.                                   01340007
           move x'09'              to cr-tab1
                                      cr-tab2

           PERFORM UNTIL EOF                                            01370000
              READ MICR-DRAFT-FILE                                      01380000
              IF MICR-STATUS = '00'                                     01390000
011703           IF M420C-FORM = '420C' AND 
011703              M420C-CHECK-DATE NOT = '01/15/03'
                    PERFORM 100-GET-BARCODE THRU 100-EXIT               01410000
                    PERFORM 200-WRITE-DRAFT-COPY THRU 200-EXIT          01420000
                    PERFORM 300-WRITE-LETTER THRU 300-EXIT              01430000
                 END-IF                                                 01440000
              END-IF                                                    01450000
           END-PERFORM                                                  01460000
                                                                        01470000
           STOP RUN.                                                    01480000
                                                                        01490000
                                                                        01500000
       EJECT                                                            01510000
      *---------------------------------------------------------------  01520000
       100-GET-BARCODE.                                                 01530000
      *---------------------------------------------------------------  01540000
           MOVE M420C-CLAIM-NO TO BC-CLAIM-NO                           01550000
           MOVE M420C-CERT-NO  TO BC-CERT-NO                            01560000
           ADD 1 TO BC-SEQ                                              01570000
                                                                        01580000
TEST****   DISPLAY CID-BARCODE                                          01590008
                                                                        01600000
           MOVE CID-BARCODE TO AGEB16-BC-IN                             01610000
           MOVE LENGTH OF CID-BARCODE TO AGEB16-BC-LEN                  01620000
           CALL 'AGEB16' USING AGEB16-BC-LEN                            01630000
                               AGEB16-BC-IN                             01640000
                               AGEB16-BC-OUT.                           01650000
                                                                        01660000
           IF AGEB16-BC-LEN = +128                                      01670000
              DISPLAY 'BARCODE ROUTINE ERROR: ' CID-BARCODE.            01680000
                                                                        01690000
           MOVE AGEB16-BC-OUT TO DR-BARCODE.                            01700000
                                                                        01710000
       100-EXIT.                                                        01720000
           EXIT.                                                        01730000
                                                                        01740000
                                                                        01750000
       EJECT                                                            01760000
      *---------------------------------------------------------------  01770000
       200-WRITE-DRAFT-COPY.                                            01780000
      *---------------------------------------------------------------  01790000
           MOVE M420C-SEQ-NUMBER  TO DR-SEQ                             01800000
           MOVE M420C-AMOUNT-PAID TO DR-AMT1                            01810000
           MOVE M420C-AMOUNT-PAID TO DR-AMT2                            01820000
           PERFORM 800-ADJ-ADDRESS THRU 800-EXIT                        01830001
           MOVE MICR-RECORD   TO DR-MICR-REC                            01840000
           WRITE DRAFT-COPY FROM DRAFT-RECORD                           01850000
           .                                                            01860000
       200-EXIT.                                                        01870000
           EXIT.                                                        01880000
                                                                        01890000
                                                                        01900000
       EJECT                                                            01910000
      *---------------------------------------------------------------  01920000
       300-WRITE-LETTER.                                                01930000
      *---------------------------------------------------------------  01940000
           MOVE SPACES TO LETTER-RECORD                                 01950000
                                                                        01960000
           MOVE M420C-PAYEE-NAME     TO LR-PAYEE-NAME                   01970000
           MOVE M420C-PAYEE-ADDRESS1 TO LR-PAYEE-ADDR1                  01980000
           MOVE M420C-PAYEE-ADDRESS2 TO LR-PAYEE-ADDR2                  01990000
           MOVE M420C-PAYEE-ADDRESS3 TO LR-PAYEE-ADDR3                  02000000
           MOVE M420C-PAYEE-ADDRESS4 TO LR-PAYEE-ADDR4                  02010000
                                                                        02020000
PEMMOD*    THE FOLLOWING CODE IS NOT NEEDED, IT IS ALREADY DONE         02021011
PEMMOD*        IN PARAGRAPH 800-ADJ-ADDRESS                             02022011
PEMMOD*                                                                 02023011
PEMMOD*    IF M420C-PAYEE-ADDRESS4 = SPACE                              02030011
PEMMOD*       IF M420C-PAYEE-ADDRESS3 = SPACE                           02040011
PEMMOD*          MOVE M420C-PAYEE-ZIP-CODE TO LR-PAYEE-ADDR2(31:9)      02050011
PEMMOD*       ELSE                                                      02060011
PEMMOD*          MOVE M420C-PAYEE-ZIP-CODE TO LR-PAYEE-ADDR3(31:9)      02070011
PEMMOD*    ELSE                                                         02080011
PEMMOD*       MOVE M420C-PAYEE-ZIP-CODE TO LR-PAYEE-ADDR4(31:9).        02090011
                                                                        02100001
           MOVE M420C-MEMBER-NAME  TO WS-WORK                           02110001
           PERFORM 810-SET-PERIOD THRU 810-EXIT                         02120001
           MOVE WS-WORK TO LR-MEMBER-NAME                               02130001
                                                                        02140001
           MOVE M420C-LOAN-NUMBER  TO WS-WORK                           02150001
           PERFORM 810-SET-PERIOD THRU 810-EXIT                         02160001
           MOVE WS-WORK TO LR-LOAN-NO                                   02170001
                                                                        02180001
           MOVE M420C-CLAIM-NO  TO LR-CLAIM-NO                          02190005
           MOVE AGEB16-BC-OUT   TO LR-BARCODE                           02200005
                                                                        02211406
           IF M420C-PLAN-CODE = 'A&H '                                  02211506
              IF M420C-LOAN-NUMBER = SPACE                              02212006
                 MOVE 'AH2 ' TO LR-LETTER-ID                            02213006
              ELSE                                                      02213106
                 MOVE 'AH1 ' TO LR-LETTER-ID                            02213206
           ELSE                                                         02213306
              MOVE 'LIFE' TO LR-LETTER-ID.                              02213806
                                                                        02214006
           WRITE COVER-LETTER FROM LETTER-RECORD.                       02250005
           if (m420c-plan-code = 'A&H ')
              and (m420c-payment-type = 'F')
              perform 350-write-comment thru 350-exit
           end-if

           .                                                            02270000
       300-EXIT.                                                        02280000
           EXIT.                                                        02290000
                                                                        02300001
       350-write-comment.

pemtst     MOVE SPACES TO comment-record
           MOVE 'SURVEY.DOC'    TO CR-FILE-NAME
           MOVE X'09'           TO CR-TAB1
                                   CR-TAB2
           MOVE M420C-CLAIM-NO  TO cR-CLAIM-NO                          02190005
           MOVE cid-barcode     TO cR-BARCODE                           02200005
           WRITE comment-card-rec FROM comment-RECORD                   02250005
           .                                                            02270000
       350-EXIT.                                                        02280000
           EXIT.                                                        02290000
                                                                        02310001
       EJECT                                                            02320001
      *---------------------------------------------------------------  02330001
       800-ADJ-ADDRESS.                                                 02340001
      *---------------------------------------------------------------  02350001
                                                                        02351009
           IF M420C-MEMBER-ADDRESS4 = SPACE                             02352009
              IF M420C-MEMBER-ADDRESS3 = SPACE                          02353009
                 MOVE M420C-MEMBER-ZIP-CODE                             02354009
                   TO M420C-MEMBER-ADDRESS2(26:5)                       02354109
              ELSE                                                      02355009
                 MOVE M420C-MEMBER-ZIP-CODE                             02355109
                   TO M420C-MEMBER-ADDRESS3(26:5)                       02355209
           ELSE                                                         02357009
              MOVE M420C-MEMBER-ZIP-CODE                                02357109
                TO M420C-MEMBER-ADDRESS4(26:5).                         02357209
                                                                        02359109
           MOVE M420C-MEMBER-ADDRESS1 TO WORK-ADDR(1)                   02360001
           MOVE M420C-MEMBER-ADDRESS2 TO WORK-ADDR(2)                   02370001
           MOVE M420C-MEMBER-ADDRESS3 TO WORK-ADDR(3)                   02380001
           MOVE M420C-MEMBER-ADDRESS4 TO WORK-ADDR(4)                   02390001
           MOVE SPACES                TO WORK-ADDR(5)                   02400001
           CALL 'ISD053' USING WORK-TABLE                               02410001
           MOVE WORK-ADDR(1) TO M420C-MEMBER-ADDRESS1                   02420001
           MOVE WORK-ADDR(2) TO M420C-MEMBER-ADDRESS2                   02430001
           MOVE WORK-ADDR(3) TO M420C-MEMBER-ADDRESS3                   02440001
           MOVE WORK-ADDR(4) TO M420C-MEMBER-ADDRESS4                   02450001
                                                                        02460001
           MOVE M420C-3RDADD-LINE1    TO WORK-ADDR(1)                   02470001
           MOVE M420C-3RDADD-LINE2    TO WORK-ADDR(2)                   02480001
           MOVE M420C-3RDADD-LINE3    TO WORK-ADDR(3)                   02490001
           MOVE SPACES                TO WORK-ADDR(4)                   02500001
           MOVE SPACES                TO WORK-ADDR(5)                   02510001
           CALL 'ISD053' USING WORK-TABLE                               02520001
           MOVE WORK-ADDR(1) TO M420C-3RDADD-LINE1                      02530001
           MOVE WORK-ADDR(2) TO M420C-3RDADD-LINE2                      02540001
           MOVE WORK-ADDR(3) TO M420C-3RDADD-LINE3                      02550001
                                                                        02551109
           IF M420C-PAYEE-ADDRESS4 = SPACE                              02551209
              IF M420C-PAYEE-ADDRESS3 = SPACE                           02551309
                 MOVE M420C-PAYEE-ZIP-CODE                              02551409
                   TO M420C-PAYEE-ADDRESS2(26:5)                        02551509
              ELSE                                                      02551609
                 MOVE M420C-PAYEE-ZIP-CODE                              02551709
                   TO M420C-PAYEE-ADDRESS3(26:5)                        02551809
           ELSE                                                         02551909
              MOVE M420C-PAYEE-ZIP-CODE                                 02552009
                TO M420C-PAYEE-ADDRESS4(26:5).                          02552109
                                                                        02560001
           MOVE M420C-PAYEE-ADDRESS1  TO WORK-ADDR(1)                   02570001
           MOVE M420C-PAYEE-ADDRESS2  TO WORK-ADDR(2)                   02580001
           MOVE M420C-PAYEE-ADDRESS3  TO WORK-ADDR(3)                   02590001
           MOVE M420C-PAYEE-ADDRESS4  TO WORK-ADDR(4)                   02600001
           MOVE SPACES                TO WORK-ADDR(5)                   02610001
           CALL 'ISD053' USING WORK-TABLE                               02620001
           MOVE WORK-ADDR(1) TO M420C-PAYEE-ADDRESS1                    02630001
           MOVE WORK-ADDR(2) TO M420C-PAYEE-ADDRESS2                    02640001
           MOVE WORK-ADDR(3) TO M420C-PAYEE-ADDRESS3                    02650001
           MOVE WORK-ADDR(4) TO M420C-PAYEE-ADDRESS4                    02660001
           .                                                            02670001
       800-EXIT.                                                        02680001
           EXIT.                                                        02690001
                                                                        02700000
                                                                        02710001
       EJECT                                                            02720001
      *---------------------------------------------------------------  02730001
       810-SET-PERIOD.                                                  02740001
      *---------------------------------------------------------------  02750001
           IF WS-WORK = SPACES                                          02751003
              GO TO 810-EXIT.                                           02752003
                                                                        02753003
           MOVE 'NO ' TO SET-PERIOD-SW                                  02760001
                                                                        02770001
           PERFORM VARYING SUB FROM 1 BY 1                              02780001
                     UNTIL SUB = 30 OR PERIOD-IS-SET                    02790001
                IF WS-WORK(SUB:2) = '  '                                02800001
                   MOVE '. ' TO WS-WORK(SUB:2)                          02810001
                   SET PERIOD-IS-SET TO TRUE                            02820002
               END-IF                                                   02830001
           END-PERFORM                                                  02840001
           .                                                            02850001
       810-EXIT.                                                        02860001
           EXIT.                                                        02870001
                                                                        02880001
