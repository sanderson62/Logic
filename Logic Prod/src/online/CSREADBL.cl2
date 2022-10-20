      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSREADBL.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.

      ********************************************
      *   Coversheet read business logic.
      ********************************************

       environment division.

       working-storage section.
       77  s1                          pic s999 comp-3 value +0.
       77  m1                          pic s999 comp-3 value +0.

       01  filler.
           05  month-table.
               10  f                   pic x(5) value '01Jan'.
               10  f                   pic x(5) value '02Feb'.
               10  f                   pic x(5) value '03Mar'.
               10  f                   pic x(5) value '04Apr'.
               10  f                   pic x(5) value '05May'.
               10  f                   pic x(5) value '06Jun'.
               10  f                   pic x(5) value '07Jul'.
               10  f                   pic x(5) value '08Aug'.
               10  f                   pic x(5) value '09Sep'.
               10  f                   pic x(5) value '10Oct'.
               10  f                   pic x(5) value '11Nov'.
               10  f                   pic x(5) value '12Dec'.
           05  filler redefines month-table occurs 12.
               10  tbl-month           pic 99.
               10  tbl-abbr            pic xxx.

       01 w-absolute-time       pic s9(15) comp-3.
       01 w-char-date           pic x(10).
       
       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       
       01 record-key            pic x(10).
       01 lower-case               pic x(26) value
             "abcdefghijklmnopqrstuvwxyz".
       01 upper-case               pic x(26) value
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       
                                       COPY FORMREC.

       linkage section.
       
       01 dfhcommarea. 
                                       copy CSREAD-COMMAREA.

       procedure division.

           inspect bl-input-form-name converting lower-case
              to upper-case
           move bl-input-form-name     to record-key

           exec cics read
              dataset ('FORMDEFS')
              into    (form-record)
              ridfld  (record-key)
              GTEQ
              resp    (response-code)
           end-exec
    
           if (response-code = dfhresp(normal))
              and (form-name = bl-input-form-name)
              move form-name           to bl-output-form-name
              move form-desc           to bl-output-form-desc
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
                 move special-notes (s1) to bl-output-messages (s1)
              end-perform
              move comment-1           to bl-output-comment1
              move comment-2           to bl-output-comment2
              move spaces              to bl-output-message
              set bl-ok                to true
           else
              if (response-code = dfhresp(notfnd))
                 or (response-code = dfhresp(normal))
                 move bl-input-form-name to bl-output-form-name
                 move "*** Form name does not exist"
                                       to bl-output-message
              else
                 move response-code    to display-response
                 string "*** Failure: READ resp = " display-response
                        into bl-output-message
                 end-string
              end-if
              set bl-fail              to true
           end-if

           exec cics return
           end-exec. 
