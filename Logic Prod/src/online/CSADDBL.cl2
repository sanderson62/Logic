      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSADDBL.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.

      ********************************************
      *   Coversheet add new form business logic
      ********************************************

       environment division.

       working-storage section.
       77  s1                          pic s999 comp-3 value +0.
       77  m1                          pic s999 comp-3 value +0.

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

       01 absolute-time            pic s9(15) comp-3.
       01 char-date                pic x(10).
       01 default-card-limit       pic 9(4) value 2000.
       01 response-code            pic s9(8) comp.
       01 display-response         pic 9(8).
       01 highest-account-number   pic 9(5) value 99999.
       01 lowest-account-number    pic 9(5) value 1.
       01 lower-case               pic x(26) value
             "abcdefghijklmnopqrstuvwxyz".
       01 upper-case               pic x(26) value
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       
                                       COPY FORMREC.

       linkage section.
       
       01 dfhcommarea. 
                                       copy CSADD-COMMAREA.

       procedure division.

       0000-begin.
      
      *    display ' entering ADDBL '
           move spaces                 to form-record
           inspect bl-input-form-name converting lower-case
              to upper-case
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
           move bl-input-form-desc     to form-desc
           perform varying s1 from +1 by +1 until
              s1 > +8
              move bl-input-messages (s1) to special-notes (s1)
           end-perform

           move bl-input-comment1      to comment-1
           move bl-input-comment2      to comment-2

           exec cics write
              dataset ('FORMDEFS')
              ridfld  (form-name)
              from    (form-record)
              resp    (response-code)
           end-exec

           if response-code = dfhresp(normal)
              set bl-ok                to true
              move "Form addeded"      to bl-output-message
              perform 0050-build-output thru 0050-exit
           else
              move response-code       to display-response
              string "*** Failure: In WRITE resp = "
                 display-response into bl-output-message
              end-string
              set bl-fail              to true
           end-if

           exec cics return
           end-exec. 
  
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

