       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK13.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

012819******************************************************************
012819*REMARKS.                                                        *
012819*  This program accepts a user id and password from a call.      *
012819*     it next verifies it against the logic control file and     *
012819*     either passes or fails.                                    *
012819******************************************************************
012819*                   C H A N G E   L O G
012819*
012819* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012819*-----------------------------------------------------------------
012819*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012819* EFFECTIVE    NUMBER
012819*-----------------------------------------------------------------
012819* 012819 CR2018110200005   PEMA  New Program
012819******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK13   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77 ws-send-msg-size             pic s9(8) comp value 256.
       77 ws-recv-msg-size             pic s9(8) comp value 256.
       77 ws-recv-buf                  pic x(256).
       77 ws-send-buf                  pic x(256) VALUE SPACES.
       77 ws-recv-total                pic s9(8) comp value 0.
       77 ws-recv-left                 pic s9(8) comp value 0.
       77 ws-seq-num                   pic s9(8) comp value 0.
       77 ws-flags                     pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77 X1                           PIC S999 COMP-3 VALUE +0.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  ws-bin-current-dt           pic xx value low-values.

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

       01  ws-cf-key.
           05  ws-cf-comp-id           pic xxx.
           05  ws-cf-rec-type          pic x.
           05  ws-cf-user              pic xxxx.
           05  ws-cf-seq-no            pic s9(4) comp value +0.

       01  ws-return-string.
           05  ws-return-code          pic xxxxx value '0000;'.
           05  ws-return-message       pic x(45).

       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        copy ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
      ****   client-in-data must be 36 characters.  ****
         05 CLIENT-IN-DATA.
            15  CLIENT-comp-id       pic xxx.
            15  CLIENT-user-id       pic xxxx.
            15  client-user-pw       pic x(11).
            15  FILLER               PIC X(18).
            15  filler               pic x(30).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       01  var  pic x(30).

       procedure division.

           display 'SOCK13:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK13:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK13:socket name      =', lstn-name ' '
              lstn-subname

           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit
           perform 0020-verify-user    thru 0020-exit

           perform 0200-send-buffer    thru 0200-exit

           go to 0300-close-socket

           .
       0000-init.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if

      *    display ' current date/time '
      *       ws-fn-mo '/' ws-fn-da '/' ws-fn-ccyy ' - '
      *          ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           .
       0000-exit.
           exit.

       0010-init-contact.

           .
       0010-exit.
           exit.

       0020-verify-user.

           move function upper-case(client-comp-id)
                                       to ws-cf-comp-id
           move function upper-case(client-user-id)
                                       to ws-cf-user
           move '2'                    to ws-cf-rec-type
           move +0                     to ws-cf-seq-no
           
           exec cics read
              dataset       ('ELCNTL')
              ridfld        (ws-cf-key)
              into          (control-file)
              resp          (ws-response)
           end-exec

           if not resp-normal
              move '0200;'             to ws-return-code
              move 'Invalid USER ID '  to ws-return-message
              go to 0020-exit
           end-if

           if function upper-case(client-user-pw) <>
                 cf-processor-password
              move '0300;'             to ws-return-code
              move 'Invalid USER ID and password combination '
                                       to ws-return-message
              go to 0020-exit
           end-if
           move '0000;'                to ws-return-code
           move 'Authenticated'        to ws-return-message

           .
       0020-exit.
           exit.

       0200-send-buffer.

           move ws-return-string       to ws-send-buf
           display 'SOCK13:About to send      ' ws-send-buf
           display 'SOCK13:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
              display 'SOCK13:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if

           .
       0200-exit.
           exit.

       0250-socket-error.

           display "SOCK13:did not complete"
           display 'SOCK13:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK13:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK13:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code

           .
       0250-exit.
           exit.

       0300-close-socket.

      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK13:done'
           exec cics return end-exec.
           goback.

           .
       0300-exit.
           exit.


       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.
