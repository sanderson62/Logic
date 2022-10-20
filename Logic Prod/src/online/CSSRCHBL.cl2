      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSSRCHBL.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.

      ********************************************
      *   Coversheet search business logic
      ********************************************

       environment division.

       data division.

       working-storage section.
       
       01  prev-form-name       pic x(10).
       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       01 bl-index              pic 9(8) comp.
       01 max-form-name         pic x(10).
       01 name-in-range-flag    pic 9.
       01 max-entries           pic s9(8) comp value 500.
       
       01 lower-case    pic x(26) value
                  "abcdefghijklmnopqrstuvwxyz".
       01 upper-case    pic x(26) value
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
 
                                       copy FORMREC.

       linkage section.
       
       01 dfhcommarea. 
                                       copy CSSRCH-COMMAREA.

       procedure division.

      *****************************************************
      * Using the partial or full form name provided
      * by the user, start a browse.
      *
      * In order to find only the forms that match the
      * characters provided by the user, build
      * max-last-name by replacing trailing spaces
      * with Zs.  For example, if the user entered
      * "AH", we want to find all forms between
      * "AH" and "BAZZZZ..."
      *****************************************************
      
       0000-begin.

           move bl-input-form-name     to form-name
           move spaces                 to prev-form-name
	         inspect form-name converting lower-case
                                       to upper-case
           move form-name              to max-form-name
           inspect max-form-name replacing all 
                 spaces by 'Z'
    
           exec cics startbr
              dataset ('FORMDEFS')
              ridfld  (form-name)
              gteq
              resp    (response-code)
           end-exec

           if response-code = dfhresp(notfnd)
              move "No matching records found"
                                       to bl-output-message
              move 0                   to bl-output-record-count
              set bl-fail to true
              exec cics return
              end-exec
           end-if
    
           if response-code not = dfhresp(normal)
              move response-code       to display-response
              string "*** Failure: STARTBR resp = "
                 display-response      into bl-output-message
              end-string
              set bl-fail              to true
              exec cics return
              end-exec
           end-if
           
           move 1                      to name-in-range-flag
           move 0                      to bl-index

      ********************************************************
      * Read through the records until max-last-name is
      * exceeded, end of file is reached, an error occurs,
      * or 100 records are added to the array of the 
      * commarea.  For this example application, no error
      * is returned if more than 100 records meet the
      * search criteria; only the first 100 records are
      * returned.
      ********************************************************
          
           perform until name-in-range-flag = 0
              exec cics readnext
                 dataset   ('FORMDEFS')
                 into      (form-record)
                 ridfld    (form-key)
                 keylength (length of form-key)
                 resp      (response-code)
              end-exec

      *********************************************************
      * If the read response is normal, verify that the
      * last-name doesn't exceed the max-last-name.  If the
      * user provided a first initial, the record read must
      * be a match.  Return all names that meet the criteria
      * in the output array.  Duplicate keys are treated the
      * same as a normal response since duplicates are
      * allowed on the last name alternate index.
      **********************************************************
      
              evaluate response-code  
                 when dfhresp(normal)
                    if form-name < max-form-name
                       if form-name not = prev-form-name
                          add 1 to bl-index
                          move form-name
                                       to bl-output-form-name (bl-index)
                          move form-desc
                                       to bl-output-form-desc (bl-index)
                          move form-name
                                       to prev-form-name
                       end-if
                    else 
                       move 0          to name-in-range-flag
                    end-if    

                 when dfhresp(endfile)
                    move 0             to name-in-range-flag
                 when other   
                    move response-code to display-response
                    string "*** Failure: READ resp = "
                       display-response into bl-output-message
                    end-string
                    move 0             to bl-output-record-count
                    set bl-fail to true
                    exec cics return
                    end-exec
              end-evaluate
              if bl-index = 100
                 move 0                to name-in-range-flag
              end-if

           end-perform

      **********************************************************
      * If the array index is still zero, no matching records
      * were found.  Otherwise, set the record count in the
      * output.
      **********************************************************
          
           if bl-index = 0
              move "No matching records found"
                                       to bl-output-message
              move 0                   to bl-output-record-count
           else
              move spaces              to bl-output-message
              move bl-index            to bl-output-record-count
           end-if
           
           set bl-ok to true.
           exec cics return
           end-exec

           .         
