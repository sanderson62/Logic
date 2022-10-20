       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIDSNCPY.
       AUTHOR.        PABLO.
062905******************************************************************
062905*                   C H A N G E   L O G
062905*
062905* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062905*-----------------------------------------------------------------
062905*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062905* EFFECTIVE    NUMBER
062905*-----------------------------------------------------------------
062905* 062905  2005062700005    PEMA  CHANGE LOCATION OF BRANCH 
081712* 081712  IR2012081700001  PEMA  CHANGE ST AND ACCT ASSIGNMENT
062905******************************************************************
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
      *                                                                 
           SELECT CID-FILE-IN
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
                                                                        
           SELECT CID-FILE-OUT
               ASSIGN TO SYS011.
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  CID-FILE-IN
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-IN               PIC X(400).
                                                                        
       FD  CID-FILE-OUT
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-OUT              PIC X(400).
                                                                        
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER                  BINARY.                              
           05  SUB                 PIC S9(4)    VALUE +0.               
                                                                        
       01  SUN-RECORD.
           05  SUN-INS-ST.
               10  SUN-INS         PIC X.
               10  SUN-ST          PIC XX.
           05  SUN-ACCOUNT         PIC X(10).
           05  SUN-TYPE            PIC X.
062905     05  SUN-EFF-DATE.
062905         10  SUN-EFF-MO      PIC XX.
062905         10  SUN-EFF-DA      PIC XX.
062905         10  SUN-EFF-YR      PIC XX.
062905     05  FILLER              PIC X(166).
062905     05  SUN-CERT            PIC X(10).
062905     05  FILLER              PIC X(30).
062905     05  SUN-FIN-INST.
062905         10  SUN-FILL        PIC XX.
062905         10  SUN-BRANCH      PIC XXX.
062905     05  FILLER              PIC X(169).


       01  FILLER.
           05  CID-IN-CNT          PIC 9(5) VALUE ZEROS.
           05  CID-OUT-CNT         PIC 9(5) VALUE ZEROS.
           05  S0C7                PIC X        VALUE ' '.
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC XX       VALUE '00'.             
               88  EOF                          VALUE '10'.
           05  WS-WORK-CCYYMMDD      PIC 9(8).
           05  FILLER REDEFINES WS-WORK-CCYYMMDD.
               10  WS-WORK-CC      PIC XX.
               10  WS-WORK-YR      PIC XX.
               10  WS-WORK-MO      PIC XX.
               10  WS-WORK-DA      PIC XX.

       01  AGENT-TABLE.
           05  AGENT-VALUES.
               10  FILLER         PIC X(13)  VALUE '0100000542800'.
               10  FILLER         PIC X(13)  VALUE '0110000542900'.
               10  FILLER         PIC X(13)  VALUE '0120000425100'.
               10  FILLER         PIC X(13)  VALUE '0130000521700'.
               10  FILLER         PIC X(13)  VALUE '0200000644400'.
               10  FILLER         PIC X(13)  VALUE '0300000447700'.
               10  FILLER         PIC X(13)  VALUE '0500000522100'.
               10  FILLER         PIC X(13)  VALUE '0510000522100'.
               10  FILLER         PIC X(13)  VALUE '0600000850200'.
               10  FILLER         PIC X(13)  VALUE '0610000513000'.
               10  FILLER         PIC X(13)  VALUE '0620000848500'.
               10  FILLER         PIC X(13)  VALUE '0630000850000'.
               10  FILLER         PIC X(13)  VALUE '0800000543800'.
               10  FILLER         PIC X(13)  VALUE '0900000542800'.
               10  FILLER         PIC X(13)  VALUE '1300000447700'.
               10  FILLER         PIC X(13)  VALUE '1400000531800'.
               10  FILLER         PIC X(13)  VALUE '1410000531800'.
               10  FILLER         PIC X(13)  VALUE '1420000531800'.
               10  FILLER         PIC X(13)  VALUE '1700000542700'.
               10  FILLER         PIC X(13)  VALUE '1800000543800'.
               10  FILLER         PIC X(13)  VALUE '1900000644300'.
               10  FILLER         PIC X(13)  VALUE '2100000459200'.
               10  FILLER         PIC X(13)  VALUE '2110000787900'.
               10  FILLER         PIC X(13)  VALUE '2200000635600'.
               10  FILLER         PIC X(13)  VALUE '2210000635600'.
083105         10  FILLER         PIC X(13)  VALUE '2300000963900'.
               10  FILLER         PIC X(13)  VALUE '2400000531800'.
               10  FILLER         PIC X(13)  VALUE '2700000542700'.
               10  FILLER         PIC X(13)  VALUE '4000000796500'.
               10  FILLER         PIC X(13)  VALUE '4010000796501'.
               10  FILLER         PIC X(13)  VALUE '4020000796502'.
               10  FILLER         PIC X(13)  VALUE '4300000923700'.
               10  FILLER         PIC X(13)  VALUE '4310000923701'.
083105         10  FILLER         PIC X(13)  VALUE '4500000691600'.
083105         10  FILLER         PIC X(13)  VALUE '4510000691601'.
083105         10  FILLER         PIC X(13)  VALUE '4520000691601'.
083105         10  FILLER         PIC X(13)  VALUE '4600000691600'.
083105     05  AGENT-DATA REDEFINES AGENT-VALUES OCCURS 37
                                   INDEXED BY AGT-INDEX.
               10  AGENT-CERT     PIC XXX.
               10  AGENT-ACCT     PIC X(10).

           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION.
      *                                                                 
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT                     
                                                                        
           PERFORM 1000-PROCESS THRU 1000-EXIT UNTIL EOF
           DISPLAY ' RECORDS IN   ' CID-IN-CNT
           DISPLAY ' RECORDS OUT  ' CID-OUT-CNT
           CLOSE CID-FILE-IN  CID-FILE-OUT
           GOBACK
                                                                        
                                                                        
           EJECT                                                        
           .
       1000-PROCESS.                                                    
      *                                                                 
           READ CID-FILE-IN INTO SUN-RECORD
              AT END GO TO 1000-EXIT.                                   
           ADD 1 TO CID-IN-CNT

           MOVE SUN-EFF-YR             TO WS-WORK-YR
           MOVE SUN-EFF-MO             TO WS-WORK-MO
           MOVE SUN-EFF-DA             TO WS-WORK-DA
           IF WS-WORK-YR > '60'
              MOVE '19'                TO WS-WORK-CC
           ELSE
              MOVE '20'                TO WS-WORK-CC
           END-IF
           
           IF (WS-WORK-CCYYMMDD < 20050701)
                     OR
               ((SUN-TYPE NOT = '2')
               AND (WS-WORK-CCYYMMDD < 20050819)
               AND (SUN-BRANCH = '450' OR '460' OR '451' OR '452'))
              SET AGT-INDEX TO +1
              SEARCH AGENT-DATA VARYING AGT-INDEX
                AT END MOVE '9999999999' TO SUN-ACCOUNT
062905*         WHEN SUN-CERT (1:3) = AGENT-CERT (AGT-INDEX)
062905          WHEN SUN-BRANCH = AGENT-CERT (AGT-INDEX)
                   MOVE AGENT-ACCT (AGT-INDEX) TO SUN-ACCOUNT
              END-SEARCH
062905        IF SUN-BRANCH (1:1) = '4'
                 MOVE 'CO'             TO SUN-INS-ST (2:2)
              END-IF
           ELSE
081712        evaluate true
081712           when sun-branch (1:1) = '1'
081712              move 'MO'          to sun-ins-st (2:2)
081712              move '0001172001'  to sun-account
081712           when sun-branch (1:1) = '5'
081712              move 'CO'          to sun-ins-st (2:2)
081712              move '0001172000'  to sun-account
081712           when other
081712              move 'KS'          to sun-ins-st (2:2)
081712              move '0001171000'  to sun-account
081712        end-evaluate
           END-IF

           WRITE CID-RECORD-OUT FROM SUN-RECORD
           ADD 1 TO CID-OUT-CNT
           .
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
      *                                                                 
       0000-HOUSEKEEPING.                                               
      *                                                                 
                                                                        
           OPEN INPUT CID-FILE-IN
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
                                                                        
           OPEN OUTPUT CID-FILE-OUT
           .                                                            
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
