      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSSRCH.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.
      ********************************************
      *   Coversheet search for form definitions
      ********************************************
       environment division.
       data division.
       working-storage section.
       01  DFH-START PIC X(04).
      ************************************************
      * commarea passed to the business logic
      ************************************************
       01  srch-commarea.
      *                                copy CSSRCH-COMMAREA.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by CSO.                                    *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      ****************************************
      *  commarea for QUERY by Form Name
      *  (business logic input & output)
      ****************************************
           03  bl-input.
               05  bl-input-form-name  pic x(10).
           03  bl-output.
               05  bl-status           pic x.
                   88  bl-ok                 value "P".
                   88  bl-fail               value "F".
               05  bl-output-message   pic x(50).
               05  bl-output-record-count
                                       pic 9(8) comp.
               05  bl-output-record  occurs 100 times.
                   07  bl-output-form-name
                                       pic x(10).
                   07  bl-output-form-desc
                                       pic x(30).
      ************************************
      * fields used to read web data
      ************************************
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(80).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).
      ******************************************
      * symbol list text for SRCHHDR template
      ******************************************
       01 output-msg.
          05 filler                    pic x(4) value "MSG=".
          05 out-msg-text              pic x(50).
      *****************************************
      * symbol list for the SRCHROW template
      *****************************************
       01 output-data.
          05  filler                   pic x(5) value"FORM=".
          05  out-form-name            pic x(10).
          05  filler                   pic x(6) value"&DESC=".
          05  out-form-desc            pic x(30).
      *************************
      *       misc
      *************************
       01 bl-index               pic s9(8) comp.
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
           MOVE 'CSSRCH' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
       0000-begin.
          
      * exec cics web
      *      startbr formfield resp(w-resp)
      *   end-exec.
      *    MOVE 'X(f                   %  N#00000167' TO DFHEIV0
           MOVE X'582866202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303030313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
          perform 0200-read-form      thru 0200-exit until
             w-resp not = 0 
      *   dfhresp(normal)
          
      * exec cics web
      *      endbr formfield
      *   end-exec
      *    MOVE 'X,f                   !   #00000172' TO DFHEIV0
           MOVE X'582C66202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * exec cics link
      *       program  ('CSSRCHBL')
      *       commarea (srch-commarea)
      *   end-exec
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'CSSRCHBL' TO DFHEIV1
      *    MOVE '."C                   ''   #00000175' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      ***********************************************************
      * Build output document.
      ***********************************************************
           move bl-output-message      to out-msg-text
           
      * exec cics document create
      *       doctoken   (w-doctoken)
      *       template   ('CSHDR')
      *       symbollist (output-msg)
      *       listlength (length of output-msg)
      *    end-exec
           MOVE 'CSHDR'
            TO DFHEIVL0
           MOVE LENGTH OF
            output-msg TO DFHEIV16
      *    MOVE '\"D tSL               )   #00000183' TO DFHEIV0
           MOVE X'5C22442074534C2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIVL0, 
                 output-msg, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           move 1                      to bl-index
           perform bl-output-record-count times
              move bl-output-form-name (bl-index)
                                       to out-form-name
              move bl-output-form-desc (bl-index)
                                       to out-form-desc
              
      * exec cics document set
      *          doctoken   (w-doctoken)
      *          symbollist (output-data)
      *          length     (length of output-data)
      *       end-exec
           MOVE LENGTH OF
            output-data TO DFHEIV16
      *    MOVE '\(Ds L                ''   #00000195' TO DFHEIV0
           MOVE X'5C284473204C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303030313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 output-data, 
                 DFHEIV99, 
                 DFHEIV16, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              
      * exec cics document insert
      *          doctoken (w-doctoken)
      *          template ('CSROW')
      *       end-exec
           MOVE 'CSROW'
            TO DFHEIVL0
      *    MOVE '\$Dt                  (   #00000200' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIVL0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              add 1                    to bl-index
           end-perform
           
      * exec cics document insert
      *       doctoken (w-doctoken)
      *       template ('CSFTR')
      *    end-exec
           MOVE 'CSFTR'
            TO DFHEIVL0
      *    MOVE '\$Dt                  (   #00000206' TO DFHEIV0
           MOVE X'5C2444742020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIVL0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
          if bl-fail
             
      * exec cics syncpoint rollback
      *      end-exec
      *    MOVE '6"R                   !   #00000211' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
          end-if
      ****************************************
      * Send the document and return.
      ****************************************
           
      * exec cics web send
      *       doctoken(w-doctoken)
      *    end-exec
      *    MOVE 'X$D                   &   #00000217' TO DFHEIV0
           MOVE X'582444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-doctoken, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    &   #00000220' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0200-read-form.
           move spaces                 to w-form-name
           move length of w-form-name  to w-form-name-len
           move spaces                 to w-form-value
           move length of w-form-value to w-form-value-len
           
      * exec cics web readnext
      *       formfield   (w-form-name)
      *       namelength  (w-form-name-len)
      *       value       (w-form-value)
      *       valuelength (w-form-value-len)
      *       resp        (w-resp)
      *    end-exec
      *    MOVE 'X*FLVL                %  N#00000228' TO DFHEIV0
           MOVE X'582A464C564C202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303030323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 w-form-name, 
                 w-form-name-len, 
                 w-form-value, 
                 w-form-value-len
           MOVE EIBRESP  TO w-resp
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           evaluate w-resp
              when 0 
      *   dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'form_name'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                       to bl-input-form-name
                       else
                          move spaces  to bl-input-form-name
                      end-if
                 end-evaluate
              when other
                 continue
           end-evaluate
           .
       0200-exit.
           exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSSRCH' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSSRCH' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
