       IDENTIFICATION DIVISION.
       PROGRAM-ID.     TESTMAP.
      ***********************************************************
      * TEST MAPS
      ***********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       01    FILLER.
           05  MAPNAME.
               10 MP-CHR   OCCURS 8 TIMES PIC X.
           05  SETNAME.
               10 ST-CHR   OCCURS 8 TIMES PIC X.
           05 SCREEN-IMAGE.
              10 SC-CHR              OCCURS 80 TIMES PIC X.
           05 SCREEN-LENGTH         PIC S9(4)  COMP VALUE +80.
           05 SUB1                         PIC 999 VALUE 0.
           05 SUB2                         PIC 999 VALUE 0.
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA       PIC X(01).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'TESTMAP' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
       100-MAIN-CONTROL SECTION.
      ***********************************************************
      * RECEIVE SCREEN
      ***********************************************************
           
      * EXEC CICS RECEIVE
      *              INTO(SCREEN-IMAGE)
      *              LENGTH(SCREEN-LENGTH)
      *              END-EXEC.
      *    MOVE '$"I    L              ''   #00000115' TO DFHEIV0
           MOVE X'242249202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SCREEN-IMAGE, 
                 SCREEN-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
      ***********************************************************
      * CHECK FOR MAPNAME MAPSET
      ***********************************************************
           MOVE SPACES TO MAPNAME SETNAME.
           MOVE 5 TO SUB1.
           MOVE 1 TO SUB2.
           PERFORM MAP-LOOP THRU MAP-EXIT.
      ***********************************************************
      * SENT MAP
      ***********************************************************
           IF SETNAME = SPACES
               
      * EXEC CICS SEND MAP(MAPNAME)
      *                  FROM (SETNAME)
      *                  CURSOR
      *                  MAPONLY
      *                  ERASE
      *        END-EXEC
           MOVE LENGTH OF
            SETNAME
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$M    CT  E    H L F ,   #00000130' TO DFHEIV0
           MOVE X'38244D202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAPNAME, 
                 SETNAME, 
                 DFHEIV12, 
                 MAPNAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           ELSE
               
      * EXEC CICS SEND MAP(MAPNAME)
      *                  MAPSET(SETNAME)
      *                  MAPONLY
      *                  ERASE
      *        END-EXEC.
      *    MOVE '8$M     T  E    H     ,   #00000137' TO DFHEIV0
           MOVE X'38244D202020202054202045' TO DFHEIV0(1:12)
           MOVE X'202020204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAPNAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 SETNAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS RETURN END-EXEC.
      *    MOVE '.(                    &   #00000142' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'TESTMAP' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       MAP-LOOP.
           ADD 1 TO SUB1.
           IF SUB1 GREATER THAN SCREEN-LENGTH
               GO TO MAP-EXIT.
           IF SUB2 GREATER THAN 8
              MOVE 1 TO SUB2
              GO TO MAP-LOOP2.
           IF SC-CHR(SUB1) = SPACES
               MOVE 1 TO SUB2
               GO TO MAP-LOOP2.
           MOVE SC-CHR(SUB1) TO MP-CHR(SUB2).
           ADD 1 TO SUB2.
           GO TO MAP-LOOP.
       MAP-LOOP2.
           ADD 1 TO SUB1.
           IF SUB1 GREATER THAN SCREEN-LENGTH
               GO TO MAP-EXIT.
           IF SUB2 GREATER THAN 8
              GO TO MAP-EXIT.
           IF SC-CHR(SUB1) = SPACES
              GO TO MAP-EXIT.
           MOVE SC-CHR(SUB1) TO ST-CHR(SUB2).
           ADD 1 TO SUB2.
           GO TO MAP-LOOP2.
       MAP-EXIT.  EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'TESTMAP' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'TESTMAP' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
