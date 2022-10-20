       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSADDLTR.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM addarch.html

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 121802    2009122800001  PEMA  NEW PROGRAM
061421* 061421  CR2017031500001  PEMA  Update to CCM8
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.

      ************************************************
      * commarea passed to the business logic
      ************************************************
      	  
       01 srch-commarea.
                                       copy ELCLTRSPI.

061421 01  input-string                pic x(200).
       01  INPUT-FROM-FORM.
           05  IFF-CARRIER             PIC X.
           05  IFF-CLAIM-NO            PIC X(7).
           05  IFF-CERT-NO             PIC X(11).
           05  IFF-LETTER-ID           PIC XXXX.
           05  IFF-FOLLOW-UP-DT        PIC X(10).
           05  IFF-RESEND-DT           PIC X(10).
           05  IFF-NO-OF-COPIES        PIC 99.
           05  IFF-PROC-ID             PIC XXXX.
           05  IFF-COMP-ID             PIC XXX.
           05  IFF-PRINT-NOW-SW        PIC X.
           05  IFF-ENC-CD              PIC XXX.
           05  IFF-ARCHIVE-NO          PIC 9(8).
           05  IFF-REGARDING           PIC X(70).

      ************************************
      * fields used to read web data
      ************************************
      
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(160).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).

       01  WS-ELMSTR-KEY.
           05  WS-ELMSTR-COMPANY-CD    PIC X.
           05  WS-ELMSTR-CARRIER       PIC X.
           05  WS-ELMSTR-CLAIM-NO      PIC X(7).
           05  WS-ELMSTR-CERT-NO       PIC X(11).

                                       COPY ELCMSTR.

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

      *****************************************
      * symbol list for the SRCHROW template
      *****************************************

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

061421     unstring
061421        input-string(6:192) delimited by '~~'
061421        into
061421          IFF-CARRIER     
061421          IFF-CLAIM-NO    
061421          IFF-CERT-NO     
061421          IFF-LETTER-ID   
061421          IFF-FOLLOW-UP-DT
061421          IFF-RESEND-DT   
061421          IFF-NO-OF-COPIES
061421          IFF-PROC-ID     
061421          IFF-COMP-ID     
061421          IFF-PRINT-NOW-SW
061421          IFF-ENC-CD      
061421          IFF-ARCHIVE-NO  
061421          IFF-REGARDING   
061421     end-unstring
061421     INSPECT iff-regarding REPLACING ALL '~' BY ' '

           MOVE INPUT-FROM-FORM        TO BL-INPUT

PEMTST*    DISPLAY ' I F F ' INPUT-FROM-FORM
      *****************************************
      * Invoke the SEARCH business logic
      *****************************************

           exec cics link
              program('NSALTRBL')
              commarea(srch-commarea)
           end-exec.

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
                    when 'clmkey'
                       if w-form-value-len not = 0
                          move w-form-value(1:w-form-value-len)
061421                           to INPUT-string
061421*                          to INPUT-FROM-FORM
                       else
                          move spaces to input-string
                       end-if
                 end-evaluate
              when other
                 continue
           end-evaluate.
      *    display ' input string ' input-string
           .
       read-form-exit.		
