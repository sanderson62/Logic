00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ELLDRCN.                            
00009                                                                   
00010 *AUTHOR.     SUZAN VUKOV.                                         
00030                                                                   
00031  ENVIRONMENT DIVISION.                                            
00032  INPUT-OUTPUT SECTION.                                            
00033  FILE-CONTROL.                                                    
00034                                                                   
00093      SELECT OUTPUT-FILE   ASSIGN TO SYS010 
                                ORGANIZATION IS LINE SEQUENTIAL.           

           SELECT INPUT-FILE    ASSIGN TO SYS011 
                                ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE     ASSIGN TO SYS019.
00099                                                                   
00484                                                                   
00485  DATA DIVISION.                                                   
00486                                                                   
00487  FILE SECTION.                                                    
00488                                                                   
00540  FD  OUTPUT-FILE                                                 
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
00541                                                                   
00542      COPY ELCRCON.                                                
00488                                                                   
00540  FD  INPUT-FILE                                                  
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  INPUT-FILE-REC                   PIC X(129).

       FD  DISK-DATE 

           COPY ELCDTEFD.
00541                                                                   
00543                                                                   
00818  WORKING-STORAGE SECTION.                                         
00819                                                                   
00824  01  FILLER.                                                      
00825      05  EOF-SW                       PIC X(01)     VALUE SPACE.
               88 EOF                                     VALUE 'Y'.
           05  WS-ABEND-MESSAGE             PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS         PIC X(02)     VALUE ZEROS.

       01  FILLER                           COMP SYNC.
           05  PGM-SUB                      PIC S9(04)    VALUE +585.

       01  FILLER                           COMP-3.
00825      05  OUTPUT-RECORD-CNT            PIC S9(07)    VALUE +0. 
           05  WS-RETURN-CODE               PIC S9(03)    VALUE +0.
           05  WS-ZERO                      PIC S9(01)    VALUE +0.

      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.

                     COPY ELCDTEVR.
00828                                                                   
00829      EJECT                                                        
00830  PROCEDURE DIVISION.                                              
00831                                                                   
      ****************READ DISK-DATE FILE
       0000-DATE-CARD-READ. COPY ELCDTERX.

       0050-MAIN.

00832      OPEN INPUT  INPUT-FILE                                         
                OUTPUT OUTPUT-FILE


           PERFORM 0100-READ THRU 0100-EXIT 
               UNTIL EOF

           CLOSE INPUT-FILE
                 OUTPUT-FILE

           DISPLAY 'OUTPUT RECORD COUNT IS ' OUTPUT-RECORD-CNT
           DISPLAY WS-ABEND-MESSAGE
           DISPLAY 'File Status is ' WS-ABEND-FILE-STATUS


           .
00833                                                                   
00834  0100-READ.                                                       

00835      READ INPUT-FILE                                               
00836          AT END 
                   SET EOF TO TRUE
                   GO TO 0100-EXIT
           END-READ                                  

           PERFORM 0200-WRITE-OUTPUT          THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.


       0200-WRITE-OUTPUT.

           DISPLAY INPUT-FILE-REC
           MOVE INPUT-FILE-REC TO CHECK-RECONCILIATION
           DISPLAY 'output ' CHECK-RECONCILIATION  
           DISPLAY 'company cd is ' DTE-CLASIC-COMPANY-CD
           MOVE DTE-CLASIC-COMPANY-CD         TO RC-COMPANY-CD
           DISPLAY 'output w company cd ' CHECK-RECONCILIATION
01032      WRITE CHECK-RECONCILIATION                      
           ADD +1 TO OUTPUT-RECORD-CNT

           . 

       0200-EXIT.
           EXIT.


       ABEND-PGM. COPY ELCABEND.
