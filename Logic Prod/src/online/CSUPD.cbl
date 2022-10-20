      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSUPD.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.
      ********************************************
      *   Coversheet update. get and create document
      ********************************************
041417******************************************************************
041417*                   C H A N G E   L O G
041417*
041417* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
041417*-----------------------------------------------------------------
041417*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041417* EFFECTIVE    NUMBER
041417*-----------------------------------------------------------------
041417* 041417  CR2016022400002  PEMA  TPE/BPE Upgrade
041417******************************************************************
       environment division.
       data division.
       working-storage section.
       01  DFH-START PIC X(04).
      ************************************************
      * commarea passed to the business logic
      ************************************************
       01 upd-commarea.
      *                                copy CSUPD-COMMAREA.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      *****************************************
      *  commarea for UPDATE Form
      *  (business logic input & output)
      *****************************************
                 03  bl-input.
               05  bl-input-form-name         pic x(10).
               05  bl-input-form-month        pic xxx.
               05  bl-input-direction         pic xxx.
               05  bl-input-form-desc         pic x(30).
               05  bl-input-messages occurs 8 pic x(75).
               05  bl-input-comment1          pic x(95).
               05  bl-input-comment2          pic x(95).
           03  bl-output.
               05  bl-status      pic x.
                   88  bl-ok      value "P".
                   88  bl-fail    value "F".
               05  bl-output-message           pic x(50).
               05  bl-output-form-name         pic x(10).
               05  bl-output-form-month        pic xxx.
               05  bl-output-form-desc         pic x(30).
               05  bl-output-messages occurs 8 pic x(75).
               05  bl-output-comment1          pic x(95).
               05  bl-output-comment2          pic x(95).
      ************************************
      * fields used to read web data
      ************************************
       01  w-form-name       pic x(10).
       01  w-form-value      pic x(100).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).
041417 01  w-template-name   pic x(48) value spaces.
      ****************************************
      * symbol list for the DETAIL template
      ****************************************
       01 output-data.
          05 filler                pic x(5) value "FORM=".
          05 out-form-name         pic x(10).
          05 filler                pic x(7) value "&FORM1=".
          05 out-form1             pic x(10).
          05 filler                pic x(6) value "&FMTH=".
          05 out-form-month        pic xxx.
          05 filler                pic x(6) value "&DESC=".
          05 out-desc              pic x(30).
          05 filler                pic x(7) value "&MESS1=".
          05 out-msgl1             pic x(75).
          05 filler                pic x(7) value "&MESS2=".
          05 out-msgl2             pic x(75).
          05 filler                pic x(7) value "&MESS3=".
          05 out-msgl3             pic x(75).
          05 filler                pic x(7) value "&MESS4=".
          05 out-msgl4             pic x(75).
          05 filler                pic x(7) value "&MESS5=".
          05 out-msgl5             pic x(75).
          05 filler                pic x(7) value "&MESS6=".
          05 out-msgl6             pic x(75).
          05 filler                pic x(7) value "&MESS7=".
          05 out-msgl7             pic x(75).
          05 filler                pic x(7) value "&MESS8=".
          05 out-msgl8             pic x(75).
          05 filler                pic x(7) value "&COMM1=".
          05 out-comm1             pic x(95).
          05 filler                pic x(7) value "&COMM2=".
          05 out-comm2             pic x(95).
          05 filler                pic x(5) value "&MSG=".
          05  out-message          pic x(50).
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
           MOVE 'CSUPD' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
       0000-get-started.
      *    display ' entering updweb '
           
      * exec cics web
      *       startbr formfield resp(w-resp)
      *    end-exec
      *    MOVE 'X(f                   &  N#00000200' TO DFHEIV0
           MOVE X'582866202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           perform 0200-read-form      thru 0200-exit until
              w-resp not = 0 
      *   dfhresp(normal)
           
      * exec cics web
      *        endbr formfield
      *    end-exec.
      *    MOVE 'X,f                   #   #00000205' TO DFHEIV0
           MOVE X'582C66202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
      *    display ' bl input ' bl-input
           
      * exec cics link
      *       program  ('CSUPDBL')
      *       commarea (upd-commarea)
      *    end-exec.
           MOVE LENGTH OF
            upd-commarea
             TO DFHEIV11
           MOVE 'CSUPDBL' TO DFHEIV1
      *    MOVE '."C                   (   #00000209' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 upd-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           move bl-output-message      to out-message
           move bl-output-form-name    to out-form-name
                                          out-form1
           move bl-output-form-month   to out-form-month
           move bl-output-form-desc    to out-desc
           move bl-output-messages (1) to out-msgl1
           move bl-output-messages (2) to out-msgl2
           move bl-output-messages (3) to out-msgl3
           move bl-output-messages (4) to out-msgl4
           move bl-output-messages (5) to out-msgl5
           move bl-output-messages (6) to out-msgl6
           move bl-output-messages (7) to out-msgl7
           move bl-output-messages (8) to out-msgl8
           move bl-output-comment1     to out-comm1
           move bl-output-comment2     to out-comm2
      *    display ' out data ' output-data
041417     move 'CSDETAIL'             to w-template-name
           
      * exec cics document create
      *       doctoken   (w-doctoken)
041417*       template   (w-template-name)
      *       symbollist (output-data)
      *       listlength (length of output-data)
      *    end-exec
           MOVE LENGTH OF
            output-data TO DFHEIV16
      *    MOVE '\"D tSL               )   #00000230' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 w-template-name, 
                 output-data, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if bl-fail
                
      * exec cics syncpoint rollback
      *         end-exec
      *    MOVE '6"R                   !   #00000237' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           
      * exec cics web send
      *       doctoken (w-doctoken)
      *    end-exec
      *    MOVE 'X$D                   *   #00000240' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202A20' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    ''   #00000243' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0200-read-form.
           move length of w-form-name  to w-form-name-len
           move length of w-form-value to w-form-value-len
           
      * exec cics web readnext
      *       formfield   (w-form-name)
      *       namelength  (w-form-name-len)
      *       value       (w-form-value)
      *       valuelength (w-form-value-len)
      *       resp        (w-resp)
      *    end-exec
      *    MOVE 'X*FLVL                &  N#00000249' TO DFHEIV0
           MOVE X'582A464C564C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *    display ' w resp      ' w-resp
      *    display ' w form name ' w-form-name
      *    display ' w form name length ' w-form-name-len
      *    display ' w form value ' w-form-value
      *    display ' w form length ' w-form-value-len
           evaluate w-resp
              when 0 
      *   dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'form_nme'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-form-name
                    when 'form_mth'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-form-month
                    when 'form_nav'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-direction
                    when 'formdesc'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-form-desc
                    when 'msgl1'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (1)
                    when 'msgl2'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (2)
                    when 'msgl3'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (3)
                    when 'msgl4'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (4)
                    when 'msgl5'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (5)
                    when 'msgl6'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (6)
                    when 'msgl7'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (7)
                    when 'msgl8'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-messages (8)
                    when 'comm1'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-comment1
                    when 'comm2'
                       move w-form-value(1:w-form-value-len)
                                       to bl-input-comment2
                 end-evaluate
              when other
                 continue
           end-evaluate
           .
       0200-exit.
           exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSUPD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSUPD' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
