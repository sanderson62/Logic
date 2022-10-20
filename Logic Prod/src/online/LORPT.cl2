       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   LORPT.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM /LOANOFFICER/INDEX.HTML

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111810                   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   
       working-storage section.
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.


       01  S1                          PIC S999 VALUE +0 COMP-3.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

      ************************************************
      * commarea passed to the business logic
      ************************************************
      	  
       01  BL-COMMAREA.
           03  BL-OUTPUT OCCURS 200.
               05  BL-CARRIER          PIC X.
               05  BL-STATE            PIC XX.
               05  BL-ACCOUNT          PIC X(10).
               05  BL-EFF-DT           PIC X(10).
               05  BL-CERT-NO          PIC X(11).
               05  BL-ISS-CAN          PIC X(10).
               05  BL-LOAN-OFF         PIC X(5).
               05  BL-BATCH-NO         PIC X(6).
               05  BL-ACCT-NAME        PIC X(30).
           03  BL-COUNT                PIC 999.
           03  BL-STATUS.
               88  BL-OK		          VALUE "P".
               88  BL-FAIL		        VALUE "F".
               05  BL-MESSAGE          PIC X(50).

      ************************************
      * fields used to read web data
      ************************************
      
       01  w-form-name       pic x(80).
       01  w-form-value      pic x(80).
       01  w-form-name-len   pic s9(8) comp.
       01  w-form-value-len  pic s9(8) comp.
       01  w-resp            pic s9(8) comp.
       01  w-doctoken        pic x(16).

      * COMP ID TPE REGION   GROUP NAME       HOST          HTTP PORT
      *
      *  CID      CID1P      BatchClaims     ntcso3            7001
      *  CID      MDOFF      BatchClaims     ntnapersofttst    7003
      *  CID      CID1T      BatchClaims     ntnapersofttst    6002
      *  CID      PAUL       BatchClaims     ntnapersofttst    5002
      *  DCC      CID1P      BatchDCCClaims  ntcso3            7001
      *  DCC      MDOFF      BatchDCCClaims  ntnapersofttst    7003
      *  DCC      CID1T      BatchDCCClaims  ntnapersofttst    6002
      *  DCC      PAUL       BatchDCCClaims  ntnapersofttst    5002

      ******************************************
      * symbol list text for LOHDR template
      ******************************************

       01  WS-PROD-LOHDR. 
           05  F                       PIC X(7)  VALUE "PGMDT=".
           05  LOHDR-DT                PIC X(10) VALUE ' '.

      ******************************************
      * symbol list text for LOBOD template
      ******************************************

       01 WS-PROD-LOBOD.
          05 FILLER              PIC X(8) VALUE "CARRIER=".
          05 OUT-CARR            PIC X.
          05 FILLER              PIC X(7) VALUE "&STATE=".
          05 OUT-STATE           PIC XX.
          05 FILLER              PIC X(9) VALUE "&ACCOUNT=".
          05 OUT-ACCT            PIC X(10).
          05 FILLER              PIC X(7) VALUE "&EFFDT=".
          05 OUT-EFFDT           PIC X(10).
          05 FILLER              PIC X(8) VALUE "&CERTNO=".
          05 OUT-CERTNO          PIC X(11).
          05 FILLER              PIC X(8) VALUE "&ISSCAN=".
          05 OUT-ISSCAN          PIC X(10).
          05 FILLER              PIC X(9) VALUE "&LOANOFF=".
          05 OUT-LOANOFF         PIC X(5).
          05 FILLER              PIC X(9) VALUE "&BATCHNO=".
          05 OUT-BATCHNO         PIC X(6).
          05 FILLER              PIC X(10) VALUE "&ACCTNAME=".
          05 OUT-ACCTNAME        PIC X(30).

      ******************************************
      * symbol list text for LOFTR template
      ******************************************

       01  WS-PROD-LOFTR. 
           05  F                       PIC X(7)  VALUE "MSG=".
           05  LOFTR-MSG               PIC X(50) VALUE ' '.

       01  WS-VAR-SLUNIKIX.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-SL-HOST-INFO         PIC X(09) VALUE 'slunikix:'.
           05  WS-SL-PORT              PIC XXXX  VALUE '7001'.
           05  WS-SL-REST              PIC X(145) VALUE SPACES.

       01  WS-VAR-LOGICTEST.
           05  FILLER                  PIC X(09) VALUE "HOSTINFO=".
           05  WS-LT-HOST-INFO         PIC X(10) VALUE 'logictest:'.
           05  WS-LT-PORT              PIC XXXX  VALUE '6002'.
           05  WS-LT-REST              PIC X(145) VALUE SPACES.


       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.

                                       COPY ELCDATE.


01168  LINKAGE SECTION.                                                 

       01  var  pic x(30).

       procedure division.
      
PEMTST*    DISPLAY ' ENTERING LORPT '
      
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
PEMTST*       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
PEMTST*       DISPLAY ' WS KIX SYS ' WS-KIXSYS
PEMTST*       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if


           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-GREG-DATE-A-EDIT    TO LOHDR-DT
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE

      *********************
      * Receive web input
      *********************
      * NO WEB INPUT FROM THIS PROJECT YET
      *    exec cics web 
      *       startbr formfield resp(w-resp) 
      *     end-exec.
      *         
      *     perform read-form thru read-form-exit
      *        until w-resp not = dfhresp(normal).
      *             
      *     exec cics web
      *       endbr formfield
      *     end-exec.
		
      *    MOVE IFF-CARRIER             TO OT-CARRIER         
      *    MOVE IFF-CLAIM-NO            TO OT-CLMNO           
      *    MOVE IFF-CERT-NO             TO OT-CRTNO           
      *    MOVE IFF-LETTER-ID           TO OT-LETTER-ID       
      *    MOVE IFF-FOLLOW-UP-DT        TO OT-FOLLOW-UP-DT    
      *    MOVE IFF-RESEND-DT           TO OT-RESEND-DT       
      *    MOVE IFF-NO-OF-COPIES        TO OT-NO-OF-COPIES    
      *    MOVE IFF-PROC-ID             TO OT-PROC-ID         
      *    MOVE IFF-COMP-ID             TO OT-COMP-ID         
      *    MOVE IFF-PRINT-NOW-SW        TO OT-PRINT-NOW-SW    
      *    MOVE IFF-ENC-CD              TO OT-ENC-CD
      *    MOVE ZEROS                   TO OT-ARCHIVE-NO
      *    MOVE IFF-REGARDING           TO OT-REGARDING       

PEMTST*    DISPLAY ' INPUT FORM ' WS-KEY
      *****************************************
      * Invoke the report business logic
      *****************************************

           MOVE SPACES                 TO BL-COMMAREA
      *    DISPLAY ' ABOUT TO LINK TO LORPTBL '
       
           exec cics link
              program('LORPTBL')
              commarea(BL-COMMAREA)
           end-exec.
       
      *    DISPLAY ' MADE IT BACK FROM LORPTBL '
      *    IF BL-OK
PEMTST*       DISPLAY ' BL OK ' BL-COUNT
      *    END-IF

      *    PERFORM VARYING S1 FROM +1 BY +1 UNTIL
      *       S1 > 5
      *       MOVE 'X'                 TO BL-CARRIER  (S1)
      *       MOVE 'TX'                TO BL-STATE    (S1)
      *       MOVE '123'               TO BL-ACCOUNT  (S1)
      *       MOVE '11/17/2010'        TO BL-EFF-DT   (S1)
      *       MOVE S1                  TO BL-CERT-NO  (S1)
      *       MOVE 'ISS'               TO BL-ISS-CAN  (S1)
      *       MOVE 'LOAN'              TO BL-LOAN-OFF (S1)
      *    END-PERFORM

      ***********************************************************
      * Build output document.  There are three templates used
      * for this document.  LOHDR  and  LOFTR are the header
      * and footer, respectively.   LOBOD is used for the
      * actual data.  For each array entry in the business
      * logic output, set the symbol list from the array
      * entry and insert into the document using the  LOBOD
      * template.
      ***********************************************************

      *    move ' testing testing ' to bl-message
           move bl-MESSAGE        to LOFTR-MSG
	   
           exec cics document create
              doctoken   (w-doctoken)
              template   ('LOHDR')
              symbollist (WS-PROD-LOHDR)
              resp       (WS-RESPONSE)
           end-exec

      *    DISPLAY '  LOHDR DOC CREATE ' WS-RESPONSE

           move 1 to S1
      *    MOVE 5 TO BL-COUNT

           perform bl-COUNT times
              MOVE BL-CARRIER   (S1)   TO OUT-CARR         
              MOVE BL-STATE     (S1)   TO OUT-STATE   
              MOVE BL-ACCOUNT   (S1)   TO OUT-ACCT    
              MOVE BL-EFF-DT    (S1)   TO OUT-EFFDT   
              MOVE BL-CERT-NO   (S1)   TO OUT-CERTNO  
              MOVE BL-ISS-CAN   (S1)   TO OUT-ISSCAN  
              MOVE BL-LOAN-OFF  (S1)   TO OUT-LOANOFF
              MOVE BL-BATCH-NO  (S1)   TO OUT-BATCHNO
              MOVE BL-ACCT-NAME (S1)   TO OUT-ACCTNAME

      *       display ' ws-prod-lobod ' ws-prod-lobod
              exec cics document set
                 doctoken   (w-doctoken)
                 symbollist (WS-PROD-LOBOD)
                 length     (length of WS-PROD-LOBOD)
		             resp       (WS-RESPONSE)
              end-exec
      
      *       DISPLAY ' DOC SET    ' WS-RESPONSE

              exec cics document insert
                 doctoken(w-doctoken)
                 template('LOBOD')
		             resp   (WS-RESPONSE)
              end-exec

      *       DISPLAY ' LOBOD DOC INSERT ' WS-RESPONSE

      
              add 1 to S1
      
           end-perform

           exec cics document set
              doctoken   (w-doctoken)
              symbollist (WS-PROD-LOFTR)
              length     (length of WS-PROD-LOFTR)
		          resp       (WS-RESPONSE)
           end-exec

      *   DISPLAY ' LOFTR DOC SET ' WS-RESPONSE

           exec cics document insert
              doctoken(w-doctoken)
              template('LOFTR')
		          resp   (WS-RESPONSE)
           end-exec

      *    DISPLAY '  LOFTR DOC INSERT ' WS-RESPONSE

      ****************************************
      * Send the document and return.
      ****************************************
      	   
           exec cics web send
              doctoken(w-doctoken)
		          resp   (WS-RESPONSE)
           end-exec.		

PEMTST*    DISPLAY ' WEB SEND  ' WS-RESPONSE

PEMTST*    DISPLAY ' ABOUT TO RETURN '
           exec cics
              return
           end-exec.
	   
	   
      ******************************************************
      * Read all fields of the incoming form, moving
      * each to the corresponding field of the commarea 
      * (business logic input fields). 
      ******************************************************
	   	
      *read-form.
      *    move spaces to w-form-name.
      *    move length of w-form-name to w-form-name-len.
      *    move spaces to w-form-value.
      *    move length of w-form-value to w-form-value-len.
      *    exec cics web readnext 
      *                  formfield(w-form-name)
      *                  namelength(w-form-name-len)
      *                  value(w-form-value)
      *                  valuelength(w-form-value-len)
      *                  resp(w-resp)
      *    end-exec.
      *    evaluate w-resp
      *       when dfhresp(normal)
      *          evaluate w-form-name(1:w-form-name-len)
      *             when 'carrier'
      *                if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-CARRIER
      *                                BL-CARRIER
      *                else
      *                 move spaces to IFF-CARRIER
      *                end-if
      *             when 'claim_no'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                             to IFF-CLAIM-NO
      *                                BL-CLAIM-NO
      *                else
      *                 move spaces  to IFF-CLAIM-NO
      *                end-if
      *             when 'cert_no'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-CERT-NO
      *                                   BL-CERT-NO
      *                else
      *                 move spaces  to IFF-CERT-NO
      *                end-if
      *             when 'letter_id'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-LETTER-ID
      *                                   BL-LETTER-ID
      *                else
      *                   move spaces  to IFF-LETTER-ID
      *                end-if
      *             when 'fu_date'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-FOLLOW-UP-DT
      *                                   BL-FOLLOW-UP-DT
      *                else
      *                 move spaces  to IFF-FOLLOW-UP-DT
      *                end-if
      *             when 'rs_date'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-RESEND-DT
      *                                   BL-RESEND-DT
      *                else
      *                 move spaces  to IFF-RESEND-DT
      *                end-if
      *             when 'no_copies'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-NO-OF-COPIES
      *                                   BL-NO-OF-COPIES
      *                else
      *                   move zeros   to IFF-NO-OF-COPIES
      *                end-if
      *             when 'proc_id'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-PROC-ID
      *                                   BL-PROC-ID
      *                else
      *                   move 'KMSB'  to IFF-PROC-ID
      *                end-if
      *             when 'comp_id'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-COMP-ID
      *                                   BL-COMP-ID
      *                else
      *                   move 'CID'   to IFF-COMP-ID
      *                end-if
      *             when 'prt_now'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-PRINT-NOW-SW
      *                                   BL-PRINT-NOW-SW
      *                else
      *                   move 'N'     to IFF-PRINT-NOW-SW
      *                end-if
      *             when 'enc_cd'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-ENC-CD
      *                                   BL-ENC-CD
      *                else
      *                   move '0'     to IFF-ENC-CD
      *                end-if
      *             when 'regarding'
      *              if w-form-value-len not = 0
      *                   move w-form-value(1:w-form-value-len)
      *                                to IFF-REGARDING
      *                                   BL-REGARDING
      *                else
      *                   move SPACES  to IFF-REGARDING BL-REGARDING
      *                end-if
      *          end-evaluate
      *       when other
      *          continue
      *    end-evaluate.
      *read-form-exit.		
					
				        
       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
