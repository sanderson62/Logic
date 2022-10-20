       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.  CIB010.                                             00020000
                                                                        00030000
      ******************************************************************00040000
      *                     PROGRAM DESCRIPTION                        *00050000
      ******************************************************************00060000
      *
      * THIS PROGRAM READS THE MICR DRAFT FILE. 
      *
      ******************************************************************00140000
090803*                   C H A N G E   L O G
090803*
090803* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
090803*-----------------------------------------------------------------
090803*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
090803* EFFECTIVE    NUMBER
090803*-----------------------------------------------------------------
      * DANA  NEW CR#1998020500005                                     
090803* 090803    2002100700001  SMVA  ADD DCC PROCESSING/DISK-DATE FILE
090803* 090803    2002100700001  SMVA  ADD ACCT STATE AND CLM TYPE TO MICR
090803*                                FILES FOR PRINTING STATE SPEC PROG RPTS
090803*                                SPLIT PARTIAL PAYS FROM FINAL PAYS
010904* 010904                   SMVA  FIX DRAFT ORDER # ON DRAFT COPIES AND
010904*                                ADD TO TOP OF COVER LETTERS
020604* 020604                   SMVA  ADD DRAFT ORDER # TO FORMREQ FILE    
022004* 022004                   SMVA  INCREMENT BC-SEQ SEPARATELY FOR PARTIAL
022004*                                AND FINAL PAYMENTS FOR GUNTHER MACHINE 
081004* 081004  CR2004072100002  SMVA  LPAC /DCC & SECPAY CHANGES 
011305* 011305    2004072100002  PEMA  INCREASE COMMENT LINES TO 6
032106* 032106  IR2006030600002  AJRA  ADD DCC2
052406* 052406  CR2006031600001  AJRA  ADD DCC SURVEY
062006* 062006  CR2006050100003  PEMA  ADD DCC3
042607* 042607  CR2006082200001  PEMA  FORCE LENGTH OF 28 TO BARCODE
031308* 031308  CR2008010900001  AJRA  ADD NJ APPEAL NOTICE FILE
090108* 090108  CR2007041300006  AJRA  SKIP CONTINUING FORM ON AUTO PAYMENTS
091808* 091808    2008022800002  AJRA  ADD ALASKA CHECK FORM (AKCK)
      ******************************************************************
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
               RECORD KEY IS FMICR-KEY
               FILE STATUS IS MICR-STATUS.                              00310000
                                                                        00320000
090803     SELECT DRAFT-COPIES-PART-PAY  ASSIGN TO SYS006.  
090803     SELECT DRAFT-COPIES-FINAL-PAY ASSIGN TO SYS007. 
           SELECT COVER-LETTERS          ASSIGN TO SYS008.  
           SELECT COMMENT-CARDS          ASSIGN TO SYS009 
                                         ORGANIZATION LINE SEQUENTIAL.
090803     SELECT FORM-REQUESTS          ASSIGN TO SYS010.   
090803     SELECT DISK-DATE              ASSIGN TO SYS019.
031308     SELECT NJ-NOTICE              ASSIGN TO SYS011 
031308                                   ORGANIZATION LINE SEQUENTIAL.
                                                                        00410000
                                                                        00420000
       DATA DIVISION.                                                   00430000
       FILE SECTION.                                                    00440000
                                                                        00450000
       FD  MICR-DRAFT-FILE                                              00460000
           RECORD IS VARYING FROM 363 TO 3000 CHARACTERS.               00470000
       01  FILLER.                                                      00480000
           05  FMICR-KEY      PIC X(0019).
           05  FILLER        PIC X(2981).                               00500000
       01  MICR-RECORD.                                                 00510000
           COPY MICR420C.                                               00520000
                                                                        00530000
090803 FD  DRAFT-COPIES-PART-PAY
090803     LABEL RECORDS ARE STANDARD                                   00550000
090803     RECORDING MODE IS F                                          00560000
090803     BLOCK CONTAINS 0 RECORDS.                                    00570000
010904*01  DRAFT-COPY-PART-PAY       PIC X(1345).      
011305 01  DRAFT-COPY-PART-PAY       PIC X(1415).      
                                                                        00590000
090803 FD  DRAFT-COPIES-FINAL-PAY
           LABEL RECORDS ARE STANDARD                                   00550000
           RECORDING MODE IS F                                          00560000
           BLOCK CONTAINS 0 RECORDS.                                    00570000
010904*01  DRAFT-COPY-FINAL-PAY      PIC X(1345).  
011305 01  DRAFT-COPY-FINAL-PAY      PIC X(1415).  
                                                                        00590000
       FD  COVER-LETTERS
           LABEL RECORDS ARE STANDARD                                   00610000
           RECORDING MODE IS F                                          00620000
           BLOCK CONTAINS 0 RECORDS.                                    00630000
113006 01  COVER-LETTER              PIC X(390).
010904*01  COVER-LETTER              PIC X(389).  

       FD  COMMENT-CARDS                                                00600005
           LABEL RECORDS ARE STANDARD                                   00610000
           RECORDING MODE IS F                                          00620000
           BLOCK CONTAINS 0 RECORDS.                                    00630000
052406*01  COMMENT-CARD-REC          PIC X(44).
052406 01  COMMENT-CARD-REC          PIC X(57).
                                                                        00650000
090803 FD  FORM-REQUESTS
090803     LABEL RECORDS ARE STANDARD                                   00610000
090803     RECORDING MODE IS F                                          00620000
090803     BLOCK CONTAINS 0 RECORDS.                                    00630000
020604 01  FORM-REQUEST              PIC X(136).
                                                                        00650000
                                                                        00720000
090803 FD  DISK-DATE
090803     COPY ELCDTEFD.

031308 FD  NJ-NOTICE
031308     LABEL RECORDS ARE STANDARD   
031308     RECORDING MODE IS F          
031308     BLOCK CONTAINS 0 RECORDS.    
031308 01  NJ-NOTICE-REC             PIC X(47).

       EJECT                                                            00730000
                                                                        00740000
       WORKING-STORAGE SECTION.                                         00750000
       copy "ctypes.cpy".
                                                                        00760000
090803***** WS FIELDS USED FOR DISK-DATE PROCESSING VIA ELCDTERX
090803 01  FILLER.                                                      00770000
090803     05  PGM-SUB              PIC S9(03) COMP   VALUE +521.
090803     05  WS-ABEND-MESSAGE     PIC X(80)         VALUE SPACES.  
090803     05  WS-RETURN-CODE       PIC S9(04) COMP   VALUE +0.

090803***** WS FIELDS USED FOR ABEND PROCESSING VIA ELCABEND
090803 01  FILLER.                                                      00770000
090803     05  WS-ABEND-FILE-STATUS PIC X(02)         VALUE ZEROS. 
090803     05  WS-ZERO              PIC S9(01)        VALUE +0.

       01  FILLER.                                                      00770000
           05  SUB                  PIC S9(4) BINARY.  
           05  S0C7                 PIC X(01)         VALUE SPACE. 
           05  FORCE-DUMP   REDEFINES S0C7 PIC S9 COMP-3.               00800000
           05  MICR-STATUS          PIC XX            VALUE '00'.   
               88  EOF                                VALUE '10'. 
           05  WS-WORK              PIC X(30)         VALUE SPACES.   
           05  SET-PERIOD-SW        PIC XXX  VALUE SPACES.              00840001
               88  PERIOD-IS-SET    VALUE IS 'YES'.                     00850001
           05  WORK-TABLE.                                              00860000
               10  WORK-ADDR OCCURS 5 TIMES PIC X(50).                  00870000
           05  WS-PARTIAL-SEQ       PIC 9(4)          VALUE ZERO.
           05  WS-FINAL-SEQ         PIC 9(4)          VALUE ZERO.
                                                                        00880000
       01  DRAFT-RECORD.                                                00890000
090803     05  DR-SEQ              PIC 9(5).                            00900000
           05  DR-AMT1             PIC ZZZ,ZZZ,ZZZ.99.                  00910000
           05  DR-AMT2             PIC ***,***,***.99.                  00920000
011305     05  DR-MICR-REC         PIC X(1254).                         00930000
           05  DR-BARCODE          PIC X(128).                          00940000
                                                                        00950000
       01  COMMENT-RECORD.                                              00890000
           05  CR-FILE-NAME        PIC X(10) VALUE 'SURVEY.DOC'.
           05  CR-TAB1             PIC X.                               00910000
           05  CR-CLAIM-NO         PIC X(7).                            00900000
           05  CR-TAB2             PIC X.                               00910000
           05  CR-BARCODE          PIC X(25).                           00940000
                                                                        00950000
052406 01  DCC-COMMENT-RECORD.
052406     05  DCC-CR-FILE-NAME    PIC X(10) VALUE 'DCSRVY.DOC'.
052406     05  DCC-CR-TAB1         PIC X.
052406     05  DCC-CR-CLAIM-NO     PIC X(7).
052506     05  DCC-CR-TAB2         PIC X.
052506     05  DCC-CR-BARCODE      PIC X(25).
052506     05  DCC-CR-TAB3         PIC X.
052506     05  DCC-CR-EVENT        PIC X(12).
052406
090803 01  FORM-REQUEST-RECORD.     
090803     05  FR-ACCT-STATE       PIC X(02). 
090803     05  FR-CLAIM-TYPE       PIC X(01). 
090803     05  FR-BARCODE          PIC X(128).                          00940000
020604     05  FR-DRAFT-ORDER      PIC X(05).                           00940000

031308 01  NJ-NOTICE-RECORD.
031308     05  NJ-NOTICE-STATE       PIC X(2).
031308     05  NJ-NOTICE-DRAFT-SEQ   PIC X(5).
031308     05  NJ-NOTICE-DRAFT-NO    PIC X(10).
031308     05  NJ-NOTICE-MEMBER-NAME PIC X(30).
                                                                        00950000
       01  LETTER-RECORD.                                               00960000
113006     05  LR-CARRIER          PIC X.
           05  LR-LETTER-ID        PIC X(4).                            00970005
010904     05  LR-DRAFT-ORDER      PIC X(5).                            00970005
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
       01  CID-BARCODE-2. 
           05  BC2-ENCL-CODE       PIC XX       VALUE ZERO.             01080000
           05  BC2-MAIL-CODE       PIC X        VALUE '1'.              01090000
           05  BC2-DIV-CODE        PIC X        VALUE '1'.              01100000
           05  BC2-CLAIM-NO        PIC X(7)     VALUE SPACE.            01110000
           05  BC2-CERT-NO         PIC X(10)    VALUE SPACE.            01120004
           05  BC2-SEQ             PIC 9(4)     VALUE ZERO.             01130000
                                                                        01140000
      ***  AGEB16 IS USED TO BUILD THE BARCODE.  IT CALLS EANSRC ***    01150000
       01  AGEB16-PARMS.                                                01160000
      *     05  AGEB16-BC-LEN       PIC 9(4).                           01170000
           05  AGEB16-BC-LEN       SHORT.
           05  AGEB16-BC-IN        PIC X(28).                           01180000
           05  AGEB16-BC-OUT       PIC X(128).                          01190000
                                                                        01200000
                                                                        01210000
           EJECT                                                        01220000
090803*COPY ELCDATE.
090803
090803 COPY ELCDTECX.
090803
090803 COPY ELCDTEVR.  
      *                                                                 01230000
       PROCEDURE DIVISION.                                              01240000
      *                                                                 01250000
090803     COPY ELCDTERX. 

           OPEN INPUT MICR-DRAFT-FILE                                   01260000
           IF MICR-STATUS = '00' OR '97'                                01270000
              CONTINUE                                                  01280000
           ELSE                                                         01290000
              DISPLAY 'OPEN ERROR ' MICR-STATUS ' ON MICRDRFT FILE'     01300000
              ADD +1 TO FORCE-DUMP
090803     END-IF
                                                                        01320000
090803     OPEN OUTPUT DRAFT-COPIES-PART-PAY
090803                 DRAFT-COPIES-FINAL-PAY
                       COMMENT-CARDS
                       COVER-LETTERS
090803                 FORM-REQUESTS
031308                 NJ-NOTICE

           MOVE X'09'              TO CR-TAB1
                                      CR-TAB2

           PERFORM UNTIL EOF                                            01370000
              READ MICR-DRAFT-FILE                                      01380000
              IF MICR-STATUS = '00'                                     01390000
                  IF (M420C-FORM = '420C' AND DTE-CLIENT = 'CID') 
090803               OR
090803               (M420C-FORM = 'DCC1' AND DTE-CLIENT = 'DCC')   
032106               OR (M420C-FORM = 'DCC2' AND DTE-CLIENT = 'DCC')   
062006               OR (M420C-FORM = 'DCC3' AND DTE-CLIENT = 'DCC')   
091808               OR (M420C-FORM = 'AKCK' AND DTE-CLIENT = 'CID')
022004                MOVE SPACES TO LETTER-RECORD 
                      PERFORM 100-GET-BARCODE  THRU 100-EXIT            01410000
090803                PERFORM 800-ADJ-ADDRESS  THRU 800-EXIT
                      PERFORM 300-WRITE-LETTER THRU 300-EXIT            01430000
090803            ELSE
090803                DISPLAY 'Check form and client id dont match?'
032106                      ' Form = ' M420C-FORM  
032106                      ' Client = ' DTE-CLIENT
                  END-IF                                                01440000
              END-IF                                                    01450000
           END-PERFORM                                                  01460000
           
031308     CLOSE MICR-DRAFT-FILE 
031308           DRAFT-COPIES-PART-PAY
031308           DRAFT-COPIES-FINAL-PAY
031308           COMMENT-CARDS
031308           COVER-LETTERS
031308           FORM-REQUESTS
031308           NJ-NOTICE
           
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

022004     MOVE M420C-CLAIM-NO TO BC2-CLAIM-NO 
022004     MOVE M420C-CERT-NO  TO BC2-CERT-NO                           01560000

           ADD +1 TO BC-SEQ
 
022004     EVALUATE TRUE
090108     WHEN M420C-PAYMENT-TYPE = 'P' OR 'A'
022004         ADD +1 TO WS-PARTIAL-SEQ
022004         MOVE WS-PARTIAL-SEQ   TO BC2-SEQ

022004     WHEN M420C-PAYMENT-TYPE = 'F' 
022004         ADD +1 TO WS-FINAL-SEQ
022004         MOVE WS-FINAL-SEQ TO BC2-SEQ

022004     WHEN OTHER
022004         DISPLAY 'payment type ' M420C-PAYMENT-TYPE                     
022004     END-EVALUATE
                                                                        01580000
           MOVE CID-BARCODE TO AGEB16-BC-IN                             01610000
042607*    MOVE LENGTH OF CID-BARCODE TO AGEB16-BC-LEN                  01620000
042607     MOVE +28                    TO AGEB16-BC-LEN
           CALL 'AGEB16' USING AGEB16-BC-LEN                            01630000
                               AGEB16-BC-IN                             01640000
                               AGEB16-BC-OUT
                                                                        01660000
           IF AGEB16-BC-LEN = +128                                      01670000
              DISPLAY 'BARCODE ROUTINE ERROR: ' CID-BARCODE
090803     END-IF
                                                                        01690000
022004     MOVE AGEB16-BC-OUT TO LR-BARCODE
                                                                        01710000
022004     MOVE CID-BARCODE-2 TO AGEB16-BC-IN                           01610000
042607*    MOVE LENGTH OF CID-BARCODE TO AGEB16-BC-LEN                  01620000
042607     MOVE +28                    TO AGEB16-BC-LEN
022004     CALL 'AGEB16' USING AGEB16-BC-LEN                            01630000
022004                         AGEB16-BC-IN                             01640000
022004                         AGEB16-BC-OUT
                                                                        01660000
022004     IF AGEB16-BC-LEN = +128                                      01670000
022004        DISPLAY 'BARCODE 2 ROUTINE ERROR: ' CID-BARCODE-2
022004     END-IF
                                                                        01690000
022004     MOVE AGEB16-BC-OUT TO DR-BARCODE
                                                                        01710000
090803     .
       100-EXIT.                                                        01720000
           EXIT.                                                        01730000
                                                                        01710000
      *---------------------------------------------------------------  01920000
       300-WRITE-LETTER.                                                01930000
      *---------------------------------------------------------------  01940000
                                                                        01960000
113006     MOVE M420C-DRAFT(2:1)     TO LR-CARRIER
010904     MOVE M420C-DRAFT-ORDER    TO LR-DRAFT-ORDER
           MOVE M420C-PAYEE-NAME     TO LR-PAYEE-NAME                   01970000
           MOVE M420C-PAYEE-ADDRESS1 TO LR-PAYEE-ADDR1                  01980000
           MOVE M420C-PAYEE-ADDRESS2 TO LR-PAYEE-ADDR2                  01990000
           MOVE M420C-PAYEE-ADDRESS3 TO LR-PAYEE-ADDR3                  02000000
           MOVE M420C-PAYEE-ADDRESS4 TO LR-PAYEE-ADDR4                  02010000
                                                                        02020000
           MOVE M420C-MEMBER-NAME  TO WS-WORK                           02110001
           PERFORM 810-SET-PERIOD THRU 810-EXIT                         02120001
           MOVE WS-WORK TO LR-MEMBER-NAME                               02130001
                                                                        02140001
           MOVE M420C-LOAN-NUMBER  TO WS-WORK                           02150001
           PERFORM 810-SET-PERIOD THRU 810-EXIT                         02160001
           MOVE WS-WORK TO LR-LOAN-NO                                   02170001
                                                                        02180001
           MOVE M420C-CLAIM-NO  TO LR-CLAIM-NO                          02190005
                                                                        02211406
           IF M420C-PLAN-CODE = 'A&H ' OR 'IU  ' OR 'GAP '
              IF M420C-LOAN-NUMBER = SPACE                              02212006
                 MOVE 'AH2 ' TO LR-LETTER-ID                            02213006
              ELSE                                                      02213106
                 MOVE 'AH1 ' TO LR-LETTER-ID 
090803        END-IF
           ELSE                                                         02213306
              MOVE 'LIFE' TO LR-LETTER-ID
090803     END-IF

031308     IF DTE-CLIENT = 'CID' AND M420C-ACCT-STATE = 'NJ'
031308         MOVE M420C-ACCT-STATE TO NJ-NOTICE-STATE
031308         MOVE M420C-DRAFT-ORDER TO NJ-NOTICE-DRAFT-SEQ
031308         MOVE M420C-DRAFT TO NJ-NOTICE-DRAFT-NO
031308         MOVE M420C-MEMBER-NAME TO NJ-NOTICE-MEMBER-NAME
031308         WRITE NJ-NOTICE-REC FROM NJ-NOTICE-RECORD
031308     END-IF
                                                                        02214006
           IF (M420C-PLAN-CODE = 'A&H ' OR 'IU  ' OR 'GAP ')
090803         IF (M420C-PAYMENT-TYPE = 'F')
090803             IF DTE-CLIENT = 'CID'
                       PERFORM 350-WRITE-COMMENT THRU 350-EXIT
052406             ELSE 
052406                 IF DTE-CLIENT = 'DCC'                       
052406                    PERFORM 360-WRITE-DCC-COMMENT THRU 360-EXIT
052406                 END-IF
090803             END-IF
090803         ELSE  
090803             IF (M420C-PAYMENT-TYPE = 'P')
090803                 PERFORM 370-WRITE-FORM-REQUEST THRU 370-EXIT
090803                 WRITE COVER-LETTER FROM LETTER-RECORD
090803                 PERFORM 320-CREATE-DRAFT-COPY THRU 320-EXIT
090803                 WRITE DRAFT-COPY-PART-PAY FROM DRAFT-RECORD 
090803                 GO TO 300-EXIT
090108             ELSE
090108                 IF (M420C-PAYMENT-TYPE = 'A')
090108                     WRITE COVER-LETTER FROM LETTER-RECORD
090108                     PERFORM 320-CREATE-DRAFT-COPY THRU 320-EXIT
090108                     WRITE DRAFT-COPY-PART-PAY FROM DRAFT-RECORD 
090108                     GO TO 300-EXIT
090108                 END-IF
090803             END-IF
090803         END-IF
           END-IF

           WRITE COVER-LETTER FROM LETTER-RECORD
090803     PERFORM 320-CREATE-DRAFT-COPY THRU 320-EXIT
090803     WRITE DRAFT-COPY-FINAL-PAY FROM DRAFT-RECORD 

           .                                                            02270000
       300-EXIT.                                                        02280000
           EXIT.                                                        02290000

090803*---------------------------------------------------------------  01770000
090803 320-CREATE-DRAFT-COPY.                                           01780000
090803*---------------------------------------------------------------  01790000
090803     MOVE M420C-DRAFT-ORDER  TO DR-SEQ    
090803     MOVE M420C-AMOUNT-PAID  TO DR-AMT1  
090803     MOVE M420C-AMOUNT-PAID  TO DR-AMT2 
090803     MOVE MICR-RECORD        TO DR-MICR-REC 
090803
090803     .                                                            01860000
090803 320-EXIT.                                                        01870000
090803     EXIT.                                                        01880000
                                                                        01890000
                                                                        02300001
       350-WRITE-COMMENT.

pemtst     MOVE SPACES          TO COMMENT-RECORD
           MOVE 'SURVEY.DOC'    TO CR-FILE-NAME
           MOVE X'09'           TO CR-TAB1
                                   CR-TAB2
           MOVE M420C-CLAIM-NO  TO CR-CLAIM-NO                          02190005
           MOVE CID-BARCODE     TO CR-BARCODE                           02200005
           WRITE COMMENT-CARD-REC FROM COMMENT-RECORD                   02250005
           .                                                            02270000
       350-EXIT.                                                        02280000
           EXIT.                                                        02290000

052406              
052406 360-WRITE-DCC-COMMENT.
052406
052406     MOVE SPACES          TO DCC-COMMENT-RECORD
052406     MOVE 'DCSRVY.DOC'    TO DCC-CR-FILE-NAME
052406     MOVE X'09'           TO DCC-CR-TAB1
052406                             DCC-CR-TAB2
052406                             DCC-CR-TAB3
052406     MOVE M420C-CLAIM-NO  TO DCC-CR-CLAIM-NO
052406     MOVE CID-BARCODE     TO DCC-CR-BARCODE
052406     IF M420C-PLAN-CODE = 'IU'
052406         MOVE 'unemployment' TO DCC-CR-EVENT
052406     ELSE
052406         MOVE 'a disability' TO DCC-CR-EVENT
052406     END-IF
052406     WRITE COMMENT-CARD-REC FROM DCC-COMMENT-RECORD
052406     .                                            
052406 360-EXIT.                                        
052406     EXIT.                                        
052406                                                  
090803 370-WRITE-FORM-REQUEST.

090803     MOVE SPACES TO FORM-REQUEST-RECORD
090803     MOVE M420C-ACCT-STATE     TO FR-ACCT-STATE
090803     MOVE M420C-CLAIM-TYPE     TO FR-CLAIM-TYPE
090803     MOVE DR-BARCODE           TO FR-BARCODE
020604     MOVE M420C-DRAFT-ORDER    TO FR-DRAFT-ORDER
090803
090803     WRITE FORM-REQUEST FROM FORM-REQUEST-RECORD

090803
090803     .                                                            02270000
090803 370-EXIT.                                                        02280000
090803     EXIT.                                                        02290000
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
090803        END-IF
           ELSE                                                         02357009
              MOVE M420C-MEMBER-ZIP-CODE                                02357109
                TO M420C-MEMBER-ADDRESS4(26:5)
090803     END-IF
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
                   TO M420C-PAYEE-ADDRESS3(26:5) 
090803        END-IF 
           ELSE                                                         02551909
              MOVE M420C-PAYEE-ZIP-CODE                                 02552009
                TO M420C-PAYEE-ADDRESS4(26:5)
090803     END-IF
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
              GO TO 810-EXIT
090803     END-IF
                                                                        02753003
           MOVE 'NO ' TO SET-PERIOD-SW                                  02760001
                                                                        02770001
           PERFORM VARYING SUB FROM 1 BY 1                              02780001
                     UNTIL SUB = 30 OR PERIOD-IS-SET                    02790001
               IF WS-WORK(SUB:2) = '  '    
                   MOVE '. ' TO WS-WORK(SUB:2)                          02810001
                   SET PERIOD-IS-SET TO TRUE                            02820002
               END-IF                                                   02830001
           END-PERFORM                                                  02840001
           .                                                            02850001
       810-EXIT.                                                        02860001
           EXIT.                                                        02870001

090803 ABEND-PGM SECTION. 
090803     COPY ELCABEND.
