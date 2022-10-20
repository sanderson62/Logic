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

      ************************************************
      * commarea passed to the business logic
      ************************************************
         
       01 upd-commarea.
                                       copy CSUPD-COMMAREA.

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

       procedure division.

       0000-get-started.

      *    display ' entering updweb '

           exec cics web 
              startbr formfield resp(w-resp) 
           end-exec
    
           perform 0200-read-form      thru 0200-exit until
              w-resp not = dfhresp(normal)
        
           exec cics web 
               endbr formfield 
           end-exec.

      *    display ' bl input ' bl-input

           exec cics link
              program  ('CSUPDBL')
              commarea (upd-commarea)
           end-exec.

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

           exec cics document create
              doctoken   (w-doctoken)
041417        template   (w-template-name)
              symbollist (output-data)
              listlength (length of output-data)
           end-exec

           if bl-fail
                exec cics syncpoint rollback
                end-exec
           end-if

           exec cics web send
              doctoken (w-doctoken)
           end-exec

           exec cics return
           end-exec

           .     
       0200-read-form.

           move length of w-form-name  to w-form-name-len
           move length of w-form-value to w-form-value-len

           exec cics web readnext 
              formfield   (w-form-name)
              namelength  (w-form-name-len)
              value       (w-form-value)
              valuelength (w-form-value-len)
              resp        (w-resp)
           end-exec

      *    display ' w resp      ' w-resp
      *    display ' w form name ' w-form-name
      *    display ' w form name length ' w-form-name-len
      *    display ' w form value ' w-form-value
      *    display ' w form length ' w-form-value-len

           evaluate w-resp
              when dfhresp(normal)
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
