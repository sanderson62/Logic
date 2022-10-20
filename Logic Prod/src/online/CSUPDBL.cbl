      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSUPDBL.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.
      ********************************************
      *   Coversheet update business logic
      ********************************************
       environment division.
       working-storage section.
       01  DFH-START PIC X(04).
       77  s1                          pic s999 comp-3 value +0.
       77  m1                          pic s999 comp-3 value +0.
       77  ws-stop-sw                  pic x  value ' '.
           88  i-say-stop                 value 'Y'.
       77  ws-find-sw                  pic x  value ' '.
           88  found-rec                  value 'Y'.
       01  ws-compare-key.
           05  ck-form-name            pic x(10).
           05  ck-form-month           pic 99.
       01  response-code         pic s9(8) comp.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-DUPREC                  VALUE +14.
           88  RESP-DUPKEY                  VALUE +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
       01 display-response      pic 9(8).
       01  filler.
           05  month-table.
               10  f                   pic x(5) value '01JAN'.
               10  f                   pic x(5) value '02FEB'.
               10  f                   pic x(5) value '03MAR'.
               10  f                   pic x(5) value '04APR'.
               10  f                   pic x(5) value '05MAY'.
               10  f                   pic x(5) value '06JUN'.
               10  f                   pic x(5) value '07JUL'.
               10  f                   pic x(5) value '08AUG'.
               10  f                   pic x(5) value '09SEP'.
               10  f                   pic x(5) value '10OCT'.
               10  f                   pic x(5) value '11NOV'.
               10  f                   pic x(5) value '12DEC'.
           05  filler redefines month-table occurs 12.
               10  tbl-month           pic 99.
               10  tbl-abbr            pic xxx.
       01 lower-case            pic x(26) value
             "abcdefghijklmnopqrstuvwxyz".
       01 upper-case            pic x(26) value
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
      *                                copy FORMREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            FORMREC                             *
      *                            VMOD=2.031                          *
      *                                                                *
      *   Billing system file for verbaige in overlays                 *
      *                                                                *
      *   FILE DESCRIPTION = FORM DEFINITION FILE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 900   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = FORMDEFS                  RKP=0,LEN=12   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
092612* 092312  CR2012040900002  PEMA  NEW FILE AND COPYBOOK
      *****************************************************************
        01  form-record.
            03  form-key.
                05  form-name           pic x(10).
                05  form-month          pic 99.
            03  form-desc               pic x(30).
            03  special-notes occurs 8  pic x(75).
            03  comment-1               pic x(95).
            03  comment-2               pic x(95).
            03  filler                  pic x(68).
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
       01 dfhcommarea.
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
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'CSUPDBL' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
      **********************************************************
      * Using the input account number, read the account
      * record for update.  If the read fails, build an
      * error message and return.
      **********************************************************
       0000-get-started.
      *    display ' entering updbl '
      *
      *    display '  bl-input-form-name    ' bl-input-form-name
      *    display '  bl-input-form-month   ' bl-input-form-month
      *    display '  bl-input-direction    ' bl-input-direction
      *    display '  bl-input-form-desc    ' bl-input-form-desc
      *    display '  bl-input-messages (1) ' bl-input-messages (1)
      *    display '  bl-input-messages (2) ' bl-input-messages (2)
      *    display '  bl-input-messages (3) ' bl-input-messages (3)
      *    display '  bl-input-messages (4) ' bl-input-messages (4)
      *    display '  bl-input-messages (5) ' bl-input-messages (5)
      *    display '  bl-input-messages (6) ' bl-input-messages (6)
      *    display '  bl-input-messages (7) ' bl-input-messages (7)
      *    display '  bl-input-messages (8) ' bl-input-messages (8)
      *    display '  bl-input-comment1     ' bl-input-comment1
      *    display '  bl-input-comment2     ' bl-input-comment2
           move bl-input-form-name     to form-name
           inspect bl-input-form-month converting lower-case
              to upper-case
           perform varying m1 from +1 by +1 until
              (m1 > +12)
              or (bl-input-form-month = tbl-abbr (m1))
           end-perform
           if m1 <= +12
              move tbl-month (m1)   to form-month
           else
              move 00               to form-month
           end-if
           move form-key               to ws-compare-key
           evaluate function upper-case (bl-input-direction)
              when 'FWD'
                 perform 0100-startbr  thru 0100-exit
                 perform 0200-fwd      thru 0200-exit
                 if found-rec
                    set bl-ok          to true
                    perform 0050-build-output
                                       thru 0050-exit
                 end-if
              when 'BWD'
                 perform 0100-startbr  thru 0100-exit
                 perform 0300-bwd      thru 0300-exit
                 if found-rec
                    move 'Found Prev Month ' to bl-output-message
                    set bl-ok             to true
                    perform 0050-build-output thru 0050-exit
                 end-if
              when 'DEL'
                 perform 0410-read-for-update
                                       thru 0410-exit
                 if resp-normal
                    perform 0420-delete thru 0420-exit
                    if resp-normal
                       move spaces     to bl-output
                       move bl-input-form-name
                                       to bl-output-form-name
                       move bl-input-form-month
                                       to bl-output-form-month
                       move 'Delete Successful '
                                       to bl-output-message
                       set bl-ok       to true
                    else
                       move response-code
                                       to display-response
                       string "*** Failure: DELETE resp = "
                          display-response into bl-output-message
                       end-string
                       set bl-fail     to true
                    end-if
                 else
                    move response-code to display-response
                    string "*** Failure: READUP resp = "
                       display-response into bl-output-message
                    end-string
                    set bl-fail        to true
                 end-if
              when other
                 perform 0400-read-eq  thru 0400-exit
           end-evaluate
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    &   #00000300' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0050-build-output.
           move form-name              to bl-output-form-name
           move form-desc              to bl-output-form-desc
           perform varying m1 from +1 by +1 until
              (m1 > +12)
              or (form-month = tbl-month (m1))
           end-perform
           if m1 <= +12
              move tbl-abbr (m1)    to bl-output-form-month
           else
              move 'XXX'            to bl-output-form-month
           end-if
           perform varying s1 from +1 by +1 until
              s1 > +8
              move special-notes (s1)  to bl-output-messages (s1)
           end-perform
           move comment-1              to bl-output-comment1
           move comment-2              to bl-output-comment2
           .
       0050-exit.
           exit.
       0100-startbr.
           
      * exec cics startbr
      *       dataset   ('FORMDEFS')
      *       ridfld    (form-key)
      *       resp      (response-code)
      *       GTEQ
      *    end-exec
           MOVE 'FORMDEFS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00000325' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              move response-code       to display-response
              string "*** Failure: STARTBR resp = "
                display-response into bl-output-message
              end-string
              set bl-fail              to true
              
      * exec cics return end-exec
      *    MOVE '.(                    &   #00000337' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           .
       0100-exit.
           exit.
       0200-fwd.
           perform until i-say-stop
              perform 0210-readnext    thru 0210-exit
              if resp-normal
                 if ck-form-name = form-name
                    if form-month > ck-form-month
                       set i-say-stop to true
                       set found-rec  to true
                       move 'Found Next Month '
                                       to bl-output-message
      *             else
      *                perform 0210-readnext thru 0210-exit
                    end-if
                 else
                    set i-say-stop to true
                    set found-rec  to true
                    move '*** N E W   F O R M ***'
                                       to bl-output-message
      *            set i-say-stop to true
      *             perform 0500-read-last-good-one thru 0500-exit
      *             move "*** Reached last month of Form "
      *                                to bl-output-message
                 end-if
              else
                 if resp-endfile
                    set i-say-stop to true
                    perform 0500-read-last-good-one thru 0500-exit
                    move " End of file reached  " to bl-output-message
                 else
                    set i-say-stop to true
                    move response-code    to display-response
                    string "*** Failure: READN resp = " display-response
                           into bl-output-message
                    end-string
                    set bl-fail           to true
                 end-if
              end-if
           end-perform
           .
       0200-exit.
           exit.
       0210-readnext.
           
      * exec cics readnext
      *       dataset   ('FORMDEFS')
      *       ridfld    (form-key)
      *       into      (form-record)
      *       resp      (response-code)
      *    end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV12
           MOVE 'FORMDEFS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00000384' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303030333834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV12, 
                 form-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0210-exit.
           exit.
       0300-bwd.
           perform until i-say-stop
              perform 0310-readprev    thru 0310-exit
              if resp-normal
                 if ck-form-name = form-name
                    if form-month < ck-form-month
                       set i-say-stop to true
                       set found-rec  to true
      *             else
      *                perform 0310-readprev thru 0310-exit
                    end-if
                 else
                    set i-say-stop to true
                    perform 0500-read-last-good-one thru 0500-exit
                    move "*** Reached first month of Form "
                                       to bl-output-message
                    set bl-fail        to true
                 end-if
              else
                 if resp-endfile
                    set i-say-stop to true
                    perform 0500-read-last-good-one thru 0500-exit
                    move " Top of file reached  " to bl-output-message
                 else
                    set i-say-stop to true
                    move response-code    to display-response
                    string "*** Failure: READP resp = " display-response
                           into bl-output-message
                    end-string
                    set bl-fail           to true
                 end-if
              end-if
           end-perform
           .
       0300-exit.
           exit.
       0310-readprev.
           
      * exec cics readprev
      *       dataset   ('FORMDEFS')
      *       ridfld    (form-key)
      *       into      (form-record)
      *       resp      (response-code)
      *    end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV12
           MOVE 'FORMDEFS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )  N#00000430' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV12, 
                 form-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0310-exit.
           exit.
       0400-read-eq.
           perform 0410-read-for-update thru 0410-exit
           if not resp-normal
              move response-code       to display-response
              string "*** Failure: READ resp = "
                display-response into bl-output-message
              end-string
              set bl-fail              to true
              
      * exec cics return end-exec
      *    MOVE '.(                    &   #00000447' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           move bl-input-form-desc     to form-desc
           perform varying s1 from +1 by +1 until
              s1 > +8
              move bl-input-messages (s1) to special-notes (s1)
           end-perform
           move bl-input-comment1      to comment-1
           move bl-input-comment2      to comment-2
           
      * exec cics rewrite
      *       dataset ('FORMDEFS')
      *       from    (form-record)
      *       resp    (response-code)
      *    end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV11
           MOVE 'FORMDEFS' TO DFHEIV1
      *    MOVE '&& L                  %  N#00000456' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              move "Form updated"      to bl-output-message
              set bl-ok                to true
              perform 0050-build-output thru 0050-exit
           else
              move response-code       to display-response
              string "*** Failure: REWRITE resp = "
                 display-response into bl-output-message
              end-string
              set bl-fail              to true
           end-if
           .
       0400-exit.
           exit.
       0410-read-for-update.
           
      * exec cics read
      *       dataset ('FORMDEFS')
      *       update
      *       into    (form-record)
      *       ridfld  (form-key)
      *       resp    (response-code)
      *    end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV11
           MOVE 'FORMDEFS' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00000476' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV11, 
                 form-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0410-exit.
           exit.
       0420-delete.
           
      * exec cics delete
      *       dataset ('FORMDEFS')
      *       resp    (response-code)
      *    end-exec
           MOVE 'FORMDEFS' TO DFHEIV1
      *    MOVE '&(                    &  N#00000487' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0420-exit.
           exit.
       0500-read-last-good-one.
           move ws-compare-key         to form-key
           
      * exec cics read
      *       dataset ('FORMDEFS')
      *       into    (form-record)
      *       ridfld  (form-key)
      *       resp    (response-code)
      *    end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV11
           MOVE 'FORMDEFS' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00000496' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303030343936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV11, 
                 form-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if resp-normal
              perform 0050-build-output thru 0050-exit
           else
              move response-code       to display-response
              string "*** Failure: READLG resp = "
                 display-response into bl-output-message
              end-string
              set bl-fail              to true
           end-if
           .
       0500-exit.
           exit.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSUPDBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSUPDBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
