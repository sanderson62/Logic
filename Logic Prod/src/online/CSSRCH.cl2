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

      ************************************************
      * commarea passed to the business logic
      ************************************************
         
       01  srch-commarea.
                                       copy CSSRCH-COMMAREA.

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
       

       procedure division.

       0000-begin.

          exec cics web
             startbr formfield resp(w-resp) 
          end-exec.
    
          perform 0200-read-form      thru 0200-exit until
             w-resp not = dfhresp(normal)
        
          exec cics web 
             endbr formfield 
          end-exec

           exec cics link
              program  ('CSSRCHBL')
              commarea (srch-commarea)
          end-exec

      ***********************************************************
      * Build output document.
      ***********************************************************

           move bl-output-message      to out-msg-text

           exec cics document create
              doctoken   (w-doctoken)
              template   ('CSHDR')
              symbollist (output-msg)
              listlength (length of output-msg)
           end-exec

           move 1                      to bl-index
    
           perform bl-output-record-count times
              move bl-output-form-name (bl-index)
                                       to out-form-name
              move bl-output-form-desc (bl-index)
                                       to out-form-desc

              exec cics document set
                 doctoken   (w-doctoken)
                 symbollist (output-data)
                 length     (length of output-data)
              end-exec
  
              exec cics document insert
                 doctoken (w-doctoken)
                 template ('CSROW')
              end-exec
  
              add 1                    to bl-index
           end-perform
  
           exec cics document insert
              doctoken (w-doctoken)
              template ('CSFTR')
           end-exec
  
          if bl-fail
             exec cics syncpoint rollback
             end-exec
          end-if

      ****************************************
      * Send the document and return.
      ****************************************
          
           exec cics web send
              doctoken(w-doctoken)
           end-exec

           exec cics return
           end-exec

           .
       0200-read-form.

           move spaces                 to w-form-name
           move length of w-form-name  to w-form-name-len
           move spaces                 to w-form-value
           move length of w-form-value to w-form-value-len
           exec cics web readnext 
              formfield   (w-form-name)
              namelength  (w-form-name-len)
              value       (w-form-value)
              valuelength (w-form-value-len)
              resp        (w-resp)
           end-exec

           evaluate w-resp
              when dfhresp(normal)
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
