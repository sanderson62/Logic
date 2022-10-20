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
       
                                       copy FORMREC.

       linkage section.
       
       01 dfhcommarea. 
                                       copy CSUPD-COMMAREA.

       procedure division.

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
             
           exec cics return
           end-exec

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

           exec cics startbr
              dataset   ('FORMDEFS')
              ridfld    (form-key)
              resp      (response-code)
              GTEQ
           end-exec

           if not resp-normal
              move response-code       to display-response
              string "*** Failure: STARTBR resp = "
                display-response into bl-output-message
              end-string
              set bl-fail              to true
              exec cics return end-exec
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

           exec cics readnext
              dataset   ('FORMDEFS')
              ridfld    (form-key)
              into      (form-record)
              resp      (response-code)
           end-exec

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

           exec cics readprev
              dataset   ('FORMDEFS')
              ridfld    (form-key)
              into      (form-record)
              resp      (response-code)
           end-exec

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
              exec cics return end-exec
           end-if

           move bl-input-form-desc     to form-desc
           perform varying s1 from +1 by +1 until
              s1 > +8
              move bl-input-messages (s1) to special-notes (s1)
           end-perform
           move bl-input-comment1      to comment-1
           move bl-input-comment2      to comment-2
 
           exec cics rewrite
              dataset ('FORMDEFS')
              from    (form-record)
              resp    (response-code)
           end-exec
    
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

           exec cics read
              dataset ('FORMDEFS')
              update
              into    (form-record)
              ridfld  (form-key)
              resp    (response-code)
           end-exec

           .
       0410-exit.
           exit.

       0420-delete.

           exec cics delete
              dataset ('FORMDEFS')
              resp    (response-code)
           end-exec

           .
       0420-exit.
           exit.

       0500-read-last-good-one.

           move ws-compare-key         to form-key

           exec cics read
              dataset ('FORMDEFS')
              into    (form-record)
              ridfld  (form-key)
              resp    (response-code)
           end-exec
    
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
