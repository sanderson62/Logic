       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSAASLTR.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM addasarch.html

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 071111    2011022800001  PEMA  NEW PROGRAM
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
061421* 061421  CR2017031500001  PEMA  Update to CCM8

      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.

      ************************************************
      * commarea passed to the business logic
      ************************************************
       77  bl-input-length             pic 9(04) BINARY.
      	  
       01 srch-commarea.
                                       copy ELCADLTRSPI.

       01  INPUT-FROM-FORM.
           05  IFF-COMP-ID           PIC XXX.
           05  IFF-PRINT-NOW-SW      PIC X.
           05  IFF-ARCHIVE-NO        PIC 9(08).
061421     05  IFF-FUNC              PIC X(06).
041320     05  iff-batch-no          pic x(06).
041320     05  iff-batch-seq-no      pic 9(08).
           05  IFF-PROCESS-TYPE      PIC X(07).


      ************************************
      * fields used to read web data
      ************************************
      
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(160).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).

       01 output-msg.
          05 filler              pic x(4) value "MSG=".
          05 out-msg-text        pic x(50).

       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

       procedure division.

      *********************
      * Receive web input
      *********************
      
           exec cics web 
              startbr formfield resp(w-resp) 
            end-exec.
      	         
            perform read-form thru read-form-exit
               until w-resp not = dfhresp(normal).
      	             
            exec cics web
              endbr formfield
            end-exec.

           move spaces                 to bl-input
           MOVE IFF-COMP-ID            TO BL-COMP-ID
           MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
           MOVE IFF-ARCHIVE-NO         TO BL-ARCHIVE-NO
           MOVE IFF-FUNC               TO BL-FUNC
041320     move iff-batch-no           to bl-batch-no
041320     move iff-batch-seq-no       to bl-batch-seq
091820     MOVE IFF-PROCESS-TYPE       TO BL-PROCESS-TYPE

PEMTST*    DISPLAY ' I F F ' INPUT-FROM-FORM
      *****************************************
      * Invoke the SEARCH business logic
      *****************************************

      *    DISPLAY ' BL INPUT        ' BL-INPUT

           display ' about to link to nsaasbl '
           move function length(bl-input) to bl-input-length
           display ' bl input length ' bl-input-length
           exec cics link
              program  ('NSAASBL')
              commarea (bl-input)
              length   (bl-input-length)
           end-exec.

           display ' returning from nsaasbl and about to cics return '

           exec cics
              return
           end-exec.
	   
      ******************************************************
      * Read all fields of the incoming form, moving
      * each to the corresponding field of the commarea 
      * (business logic input fields).  For a search,
      * both form fields, last_name and first_initial,
      * may be null.  In that case, set the business
      * logic input fields to spaces.
      ******************************************************
	   	
       read-form.
           move spaces to w-form-name.
           move length of w-form-name to w-form-name-len.
	         move spaces to w-form-value.
           move length of w-form-value to w-form-value-len.
           exec cics web readnext 
                         formfield(w-form-name)
                         namelength(w-form-name-len)
                         value(w-form-value)
                         valuelength(w-form-value-len)
                         resp(w-resp)
           end-exec.

           evaluate w-resp
              when dfhresp(normal)
                 evaluate w-form-name(1:w-form-name-len)
                    when 'archkey'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
                                 to INPUT-FROM-FORM
                       else
                          move spaces to INPUT-FROM-FORM
                       end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.

       read-form-exit.		
