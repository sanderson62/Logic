      *****************************************************************
      *                                                               *
      * Copyright (c) 2015 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. SOCK04BL.
      *
      *AUTHOR.    Cowtown.
      *           Colleyville, TEXAS.

      *REMARKS.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This program acts as a Socket Server and expects a key    ***
      ***  to read an account master and a compensation master and   ***
      ***  will pass back a few items. It currently is designed to   ***
      ***  work with the Agent Signature Verification process.       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

031115******************************************************************
031115*                   C H A N G E   L O G
031115*
031115* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031115*-----------------------------------------------------------------
031115*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031115* EFFECTIVE    NUMBER
031115*-----------------------------------------------------------------
031115* 031115  CR               PEMA  NEW PROGRAM
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
092118* 092118  CR2018091700001  PEMA  ADD "HOME OFFICE" OPTION
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
031115*-----------------------------------------------------------------

       environment division.
       data division.
       working-storage section.

       77  WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  WS-COMP-ID                   PIC XXX VALUE 'CID'.
       77  WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77  WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77  WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77  WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77  a1                           PIC S999 COMP-3 VALUE +0.
       77  c1                           PIC S999 COMP-3 VALUE +0.
       77  WS-BUILD-SW                  PIC X.
           88  TIME-TO-BUILD               VALUE 'Y'.
       77  WS-SAVE-ERACCT               PIC X(2000).
       77  WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77  ws-bin-eff-dt                pic xx  value low-values.
       77  ws-bin-1st-pay-dt            pic xx  value low-values.
       77  WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77  ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-bin-cert-eff-dt          pic xx   value low-values.
       77  ws-current-bin-dt           pic xx   value low-values.
       77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
       77  note-count                  pic s999 comp-3 value +0.
       77  ws-build-note-sw            pic x value ' '.
           88  finished-with-notes      value 'Y'.
       77  ws-ercnot-sw                pic x  value spaces.
           88  ercnot-startbr            value 'Y'.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
       01  cert-note-records-holder.
           05  cert-note-record occurs 300.
               10  filler              pic x(48).
               10  cnr-rest            pic x(102).

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  WS-CO-KEY.
           05  WS-CO-COMPANY-CD        PIC X.                                       
           05  WS-CO-CARRIER           PIC X.                                       
           05  WS-CO-GROUP             PIC X(6).                                    
           05  WS-CO-FIN-RESP          PIC X(10).   
           05  WS-CO-ACCOUNT           PIC X(10).
           05  WS-CO-TYPE              PIC X.

       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO           PIC X(11).

       01  WS-CZ-KEY.
           05  WS-CZ-COMPANY-CD        PIC X.                                       
           05  WS-CZ-CARRIER           PIC X.                                       
           05  WS-CZ-GROUP             PIC X(6).                                    
           05  WS-CZ-STATE             PIC XX.   
           05  WS-CZ-ACCOUNT           PIC X(10).
           05  WS-CZ-EFF-DT            PIC XX.
           05  WS-CZ-CERT-NO           PIC X(11).
           05  WS-CZ-REC-TYPE          PIC X.
           05  ws-cz-note-seq          pic s9(4) comp.

       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.

       01  ws-return-stuff.
           05  ws-am-name              pic x(30).
           05  ws-am-person            pic x(30).
           05  ws-am-control-name      pic x(30).
           05  ws-am-phone             pic x(10).
           05  ws-co-fax               pic x(10).
           05  ws-co-contact           pic x(30).

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ERCACCT.
                                        COPY ERCCOMP.
                                        COPY ERCCNOT.
                                        COPY ELCDATE.
      
       linkage section.

       01  DFHCOMMAREA.
                                       copy SOCK04-COMMAREA.

       procedure division.

           display ' Begin SOCK04BL =' SOCK-IN-DATA '='

           perform 0010-init           thru 0010-exit
           perform 0100-process-request thru 0100-exit

           GO TO cics-return

           .
       0010-init.

           exec cics
              asktime
           end-exec

           display ' eibtime ' eibtime
           display ' eibdate ' eibdate

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           perform 9700-date-link      thru 9700-exit

           IF DATE-CONVERSION-ERROR
              display ' error converting eibdate '
              MOVE LOW-VALUES          TO ws-current-bin-dt
           ELSE
              MOVE DC-BIN-DATE-1       TO ws-current-bin-dt
           end-if            

           move sock-id                to ws-comp-id

020816     evaluate true
020816        when ws-comp-id = 'AHL'
020816           MOVE X'06'            TO ws-comp-cd
020816        when ws-comp-id = 'DCC'
020816           move X'05'            to ws-comp-cd
020816        when ws-comp-id = 'VPP'
020816           move X'07'            to ws-comp-cd
062121        when ws-comp-id = 'FNL'
062121           move X'08'            to ws-comp-cd
020816        when other
020816           move X'04'            to ws-comp-cd
020816     end-evaluate

           move sock-eff-dt            to dc-greg-date-1-edit
           move '2'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if not no-conversion-error
              display ' error converting eff date '
              go to cics-return
           end-if
           move dc-bin-date-1          to ws-bin-cert-eff-dt

           .
       0010-exit.
           exit.

       0100-process-request.

           perform 0110-get-elcert     thru 0110-exit
           if resp-normal
              perform 0120-process-cert-trailer
                                       thru 0120-exit
              perform 0150-process-cert-notes
                                       thru 0150-exit
              perform 0200-process-acct-comp
                                       thru 0200-exit
           end-if

           .
       0100-exit.
           exit.

       0110-get-elcert.

      *    display ' made it to 0110-get-elcert '

           move low-values             to ws-cm-key
           move ws-comp-cd             to ws-cm-company-cd
           move sock-car               to ws-cm-carrier
           move sock-grp               to ws-cm-group
           move sock-st                to ws-cm-state
           move sock-act               to ws-cm-account
           move ws-bin-cert-eff-dt     to ws-cm-eff-dt
           move sock-cert-no           to ws-cm-cert-no

           EXEC CICS READ
                INTO    (CERTIFICATE-MASTER)
                DATASET ('ELCERT')
                RIDFLD  (WS-CM-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       0110-exit.
           exit.

       0120-process-cert-trailer.

      *    display ' made it to 0120-process cert '
           perform 0121-read-elcrtt    thru 0121-exit
           evaluate true
              when resp-normal
                 perform 0122-upd-elcrtt
                                       thru 0122-exit
              when resp-notfnd
                 perform 0123-add-elcrtt
                                       thru 0123-exit
              when other
                 display ' error cert trlr ' ws-response
                 go to cics-return
           end-evaluate

           .
       0120-exit.
           exit.

       0121-read-elcrtt.

      *    display ' made it to 0121-read elcrtt  '
           move cm-control-primary     to ws-cs-key
           move 'C'                    to ws-cs-trlr-type

           EXEC CICS READ
              UPDATE
              INTO    (CERTIFICATE-TRAILERS)
              DATASET ('ELCRTT')
              RIDFLD  (WS-CS-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0121-exit.
           exit.

       0122-upd-elcrtt.

      *    display ' made it to 0122-upd-elcrtt   '
           move sock-action-taken      to cs-agent-edit-status
           if cs-agent-edit-status = 'C' or 'S' or 'U' or 'R' or
092118                               'M' or 'N' or 'H'
              move spaces              to cs-agent-name
                                          cs-license-no
                                          cs-npn-number
           end-if
           evaluate cs-agent-edit-status
              when 'U'
                 move 'Unidentified'   to cs-agent-name
              when 'R'
                 move 'Returned'       to cs-agent-name
              when 'N'
                 move 'Accept with no commission'
                                       to cs-agent-name
092118        when 'H'
092118           move 'Home Office'    to cs-agent-name
           end-evaluate

           EXEC CICS REWRITE
              DATASET ('ELCRTT')
              FROM    (CERTIFICATE-TRAILERS)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0122-exit.
           exit.

       0123-add-elcrtt.

      *    display ' made it to 0123-add elcrtt   '

           move 'CS'                   to certificate-trailers
           move cm-control-primary     to cs-control-primary
           move 'C'                    to cs-trailer-type
           move sock-action-taken    to cs-agent-edit-status
           evaluate cs-agent-edit-status
              when 'U'
                 move 'Unidentified'   to cs-agent-name
              when 'R'
                 move 'Returned'       to cs-agent-name
              when 'N'
                 move 'Accept with no commission'
                                       to cs-agent-name
092118        when 'H'
092118           move 'Home Office'    to cs-agent-name
           end-evaluate
           EXEC CICS WRITE
              DATASET ('ELCRTT')
              FROM    (CERTIFICATE-TRAILERS)
              RIDFLD  (cs-control-primary)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0123-exit.
           exit.

       0150-process-cert-notes.

      *    display ' made it to 0150 process cert notes '
           move spaces                 to cert-note-records-holder
                                          ws-build-note-sw
                                          ws-ercnot-sw
           move cm-control-primary     to ws-cz-key
           move '1'                    to ws-cz-rec-type
           move +0                     to ws-cz-note-seq
                                          c1

           EXEC CICS STARTBR
                DATASET    ('ERCNOT')
                RIDFLD     (WS-CZ-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              set ercnot-startbr to true
      *       display ' resp normal startbr '
              perform until finished-with-notes
                 EXEC CICS READNEXT
                    DATASET    ('ERCNOT')
                    RIDFLD     (WS-CZ-KEY)
                    INTO       (cert-note-file)
                    resp       (ws-response)
                 end-exec
                 if (resp-normal)
                    and (cz-control-primary (1:33) =
                       cm-control-primary (1:33))
                    if cz-record-type = '1'
                       add +1 to c1
                       move cert-note-file
                                       to cert-note-record (c1)
                    end-if
                 else
                    set finished-with-notes to true
                 end-if
              end-perform
           end-if

           if ercnot-startbr
      *       display ' about to endbr ercnot '
              exec cics endbr
                 dataset    ('ERCNOT')
              end-exec
           end-if
           move c1                     to note-count

           if c1 = +0
              perform 0151-add-verify-agent-sig-note
                                       thru 0151-exit
           else
              if ((cnr-rest (1) (1:21) = 'AGENT CHECK SIGNATURE')
                 and (sock-action-taken = 'C'))
                               or
                 ((cnr-rest (1) (1:23) = 'MISSING AGENT SIGNATURE')
                 and (sock-action-taken = 'S'))
                               or
                 ((cnr-rest (1) (1:22) = 'UNIDENTIFIED SIGNATURE')
                 and (sock-action-taken = 'U'))
                               or
                 ((cnr-rest (1) (1:20) = 'CERTIFICATE RETURNED')
                 and (sock-action-taken = 'R'))
                               or
                 ((cnr-rest (1) (1:19) = 'REFERRED TO MANAGER')
                 and (sock-action-taken = 'M'))
                               or
                 ((cnr-rest (1) (1:25) = 'ACCEPT WITH NO COMMISSION')
                 and (sock-action-taken = 'N'))
092118                         or
092118           ((cnr-rest (1) (1:17) = 'HOME OFFICE ISSUE')
092118           and (sock-action-taken = 'H'))
                 go to 0150-exit
              else
                 perform 0152-delete-cert-notes
                                       thru 0152-exit
                 if resp-normal
                    perform 0151-add-verify-agent-sig-note
                                       thru 0151-exit
                    if resp-normal
                       perform 0153-put-back-cert-notes
                                       thru 0153-exit
                       if resp-normal
                          continue
                       else
                          display ' something wrong with put back '
                             cm-cert-no
                       end-if
                    else
                       display ' something went wrong with adding note '
                          cm-cert-no
                    end-if
                 else
                    display ' something went wrong with generic delete '
                       cm-cert-no
                 end-if
              end-if
           end-if

           .
       0150-exit.
           exit.

       0151-add-verify-agent-sig-note.

      *    display ' made it to 0151 add '
           move 'CZ'                to cert-note-file
           move cm-control-primary  to cz-control-primary
           move '1'                 to cz-record-type
           move +1                  to cz-note-sequence
           move ws-current-bin-dt   to cz-last-maint-dt
           move eibtime             to cz-last-maint-hhmmss
           move sock-proc-id        to cz-last-maint-user
           evaluate true
              when sock-action-taken = 'C'
                 MOVE 'AGENT CHECK SIGNATURE COVER SHEET GENERATED'
                                    to cz-note
              when sock-action-taken = 'S'
                 move 'MISSING AGENT SIGNATURE FORM GENERATED'
                                    to cz-note
              when sock-action-taken = 'U'
                 move 'UNIDENTIFIED SIGNATURE'
                                    to cz-note
              when sock-action-taken = 'R'
                 move 'CERTIFICATE RETURNED'
                                    to cz-note
              when sock-action-taken = 'N'
                 move 'ACCEPTED WITH NO COMMISSION'
                                    to cz-note
092118        when sock-action-taken = 'H'
092118           move 'HOME OFFICE ISSUE'
092118                              to cz-note
              when other
                 move 'REFERRED TO MANAGER'
                                    to cz-note
           end-evaluate
           exec cics write
              dataset   ('ERCNOT')
              from      (cert-note-file)
              ridfld    (cz-control-primary)
              resp      (ws-response)
           end-exec
           if not resp-normal
              display ' error-ercnot-write ' ws-response ' '
                 cz-control-primary (2:33)
           end-if

           .
       0151-exit.
           exit.

       0152-delete-cert-notes.

      *    display ' made it to 0152 '
           move cm-control-primary     to ws-cz-key
           move '1'                    to ws-cz-rec-type
           move +0                     to ws-cz-note-seq
      *    display ' about to browse delete ' ws-cz-key (2:19) ' '
      *       ws-cz-key (23:11)

           EXEC CICS STARTBR
                DATASET    ('ERCNOT')
                RIDFLD     (WS-CZ-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           IF not RESP-NORMAL
              display ' error startbr for delete ' ws-response
              go to 0152-exit
           end-if

      *    display ' good startbr '

           .
       0152-read-next.

           EXEC CICS READNEXT
              DATASET    ('ERCNOT')
              RIDFLD     (WS-CZ-KEY)
              INTO       (cert-note-file)
              resp       (ws-response)
           end-exec
           if resp-normal
      *       display ' good readnext '
              if cz-control-primary (1:33) = cm-control-primary (1:33)
      *          display ' primary equal '
                 if cz-record-type = '1'
      *             display ' rec type = 1, about to endbr '
                    exec cics endbr
                       dataset    ('ERCNOT')
                       resp       (ws-response)
                    end-exec
                    if not resp-normal
                       display ' bad endbr ' ws-response
                       go to 0152-exit
                    else
      *                display ' about to read update '
                       EXEC CICS READ
                          UPDATE
                          DATASET    ('ERCNOT')
                          RIDFLD     (WS-CZ-KEY)
                          INTO       (cert-note-file)
                          resp       (ws-response)
                       end-exec
                       if not resp-normal
                          display ' bad read update ' ws-response
                          go to 0152-exit
                       end-if
      *                display ' about to delete '
                       exec cics delete
                          dataset   ('ERCNOT')
      *                   RIDFLD     (WS-CZ-KEY)
                          resp      (ws-response)
                       end-exec
                       if not resp-normal
                          display ' bad delete ercnot ' ws-response
                          go to 0152-exit
                       else
                          display ' good delete '
                       end-if
                    end-if
                 else
                    display ' rec type not = 1 '
                 end-if
                 go to 0152-delete-cert-notes
              else
                 display ' primary not = '
              end-if
           else
              display ' error ercnot readnext delete ' ws-response
           end-if


      *    display ' about to endbr '
           exec cics endbr
              dataset    ('ERCNOT')
              resp       (ws-response)
           end-exec

           if not resp-normal
              display ' bad resp endbr ercnot ' ws-response
           else
              display ' good endbr '
           end-if

           .
       0152-exit.
           exit.

      *0152-delete-cert-notes.
      *
      *    display ' made it to 0152 '
      *    move cm-control-primary     to ws-cz-key
      *    move '1'                    to ws-cz-rec-type
      *    move +0                     to ws-cz-note-seq
      *    display ' about to generic delete ' ws-cz-key (2:19) ' '
      *       ws-cz-key (23:11)
      *    exec cics delete
      *       dataset    ('ERCNOT')
      *       keylength  (ws-cert-note-generic-key-len)
      *       ridfld     (ws-cz-key (1:34))
      *       generic
      *       resp       (ws-response)
      *    end-exec
      *
      *    display ' made it back from generic delete ' ws-response
      *
      *    .
      *0152-exit.
      *    exit.

       0153-put-back-cert-notes.

      *    display ' made it to 0153 ' note-count
           perform varying c1 from +1 by +1 until
              c1 > note-count
              move cert-note-record (c1)
                                       to cert-note-file
              add +1                   to cz-note-sequence
      *       display ' about to write ' cz-control-primary (2:19) ' '
      *          cz-control-primary (23:11) ' ' cz-note-sequence ' '
      *           cz-record-type ' ' cz-note-information
              exec cics write
                 dataset ('ERCNOT')
                 FROM    (cert-note-file)
                 ridfld  (cz-control-primary)
                 resp    (ws-response)
              end-exec
              if not resp-normal
                 display ' error-ercnot-write subsequ ' ws-response ' '
                    cz-control-primary (2:33)
                 move +999             to c1
              else
                 display ' good ercnot write ' c1
              end-if
           end-perform

          .
       0153-exit.
           exit.

       0200-process-acct-comp.

      *    display ' made it to 0200 '
           move low-values             to ws-am-key
           move ws-comp-cd             to ws-am-company-cd
           move sock-car               to ws-am-carrier
           move sock-grp               to ws-am-group
           move sock-st                to ws-am-state
           move sock-act               to ws-am-account
           move ws-bin-cert-eff-dt     to ws-am-exp-dt

           EXEC CICS STARTBR
                DATASET    ('ERACCT')
                RIDFLD     (WS-AM-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           if not resp-normal
              display ' bad startbr ' ws-response
              go to 0200-exit
           end-if

           EXEC CICS READNEXT
                INTO    (ACCOUNT-MASTER)
                DATASET ('ERACCT')
                RIDFLD  (WS-AM-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           if (resp-normal)
              and (ws-am-company-cd = ws-comp-cd)
              and (ws-am-carrier    = sock-car)
              and (ws-am-state      = sock-st)
              and (ws-am-account    = sock-act)
              continue
           else
              display ' account not found '
              go to 0200-exit
           end-if

           move spaces                 to ws-return-stuff
           move am-name                to sock-am-name
           move am-person              to sock-am-person
           move am-control-name        to sock-am-cntrl-name
           move am-tel-no              to sock-am-phone

           MOVE LOW-VALUES             TO WS-CO-KEY
           MOVE AM-COMPANY-CD          TO WS-CO-COMPANY-CD
           MOVE AM-CARRIER             TO WS-CO-CARRIER
           MOVE AM-GROUPING            TO WS-CO-GROUP
           MOVE 'A'                    TO WS-CO-TYPE

           if am-remit-to not numeric
              move zeros               to am-remit-to
           end-if
           if am-remit-to = zeros
              move 01                  to am-remit-to
           end-if
           move am-agt (am-remit-to)   to ws-co-fin-resp
           perform varying a1 from +1 by +1 until
              (a1 > +10)
              or (am-com-typ (a1) = 'C' or 'D')
           end-perform
           if a1 < +11
              move am-agt (a1)         to ws-co-account
           else
              move am-account          to ws-co-account
           end-if

           EXEC CICS READ
                INTO    (COMPENSATION-MASTER)
                DATASET ('ERCOMP')
                RIDFLD  (WS-CO-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              MOVE CO-CONTROL-NAME     TO sock-co-contact
              MOVE CO-faxno            TO sock-co-fax
           ELSE
              MOVE ' unknown     '     TO sock-co-contact
           END-IF

           .
       0200-exit.
           exit.

       cics-return.

           display 'SOCK04BL Finished ' sock-return-area
           exec cics
              return
           end-exec

           goback

           .
       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
