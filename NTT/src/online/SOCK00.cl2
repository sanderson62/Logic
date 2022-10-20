      *****************************************************************
      *                                                               *
      * Copyright (c) 2001 by Sun Microsystems, Inc.                  *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
091609******************************************************************
091609*                   C H A N G E   L O G
091609*
091609* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091609*-----------------------------------------------------------------
091609*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091609* EFFECTIVE    NUMBER
091609*-----------------------------------------------------------------
091609* 091609  CR2009082400002  PEMA  ADD USER SELECT 3
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
042612* 042612  CR2012042500004  PEMA  ADD CODE TO HANDLE AHL
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
072415* 072415  IR2015071500003  PEMA  REMOVE CLOSE STATEMENT
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
091609*-----------------------------------------------------------------
       identification division.
       program-id. SOCK00.
      *         This program is the first invoked by the SOCK
      *         transaction. This transaction must be submited
      *         by a message transmited over a stream socket.
      *         The first message format is:

      *         aaaa,bbbbb              ccc

      *         where "aaaa" is the tran id SOCK (1 to 4 characters)
      *         and "bbbbb is either "RATE " or "STATE" and ccc is either
      *         DCC or CID or any future company id

      *         Information on the originating message and
      *         remote socket is provided in the DFHCOMMAREA.

      *         As its first action this program must send a
      *         message to the requesting socket. This lets the
      *         remote program know its request has been 
      *         accepted and the transaction started.

      *         The program will make four transmissions to
      *         the initating program in response to received
      *         messages and then close the socket.

      *         This program is provided to show method and
      *         has minimal checking and error messages.
      *         The Cobol compiler, unless instructed otherwise,
      *         will hold its working storage integer data items in
      *         machine idependent form. This matches the network
      *         ordering of information. Care should be exercised
      *         to ensure that this data ordering is correct when
      *         calling system library functions or passing data 
      *         structure containing such data to functions.

       environment division.
       data division.
       working-storage section.
      *
      * program buffers
      *
       77 ws-send-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-msg-size           pic s9(8) comp value 4096.
       77 ws-recv-buf                pic x(4096).
       77 ws-send-buf                pic x(4096) VALUE SPACES.
       77 ws-recv-total              pic s9(8) comp value 0.
       77 ws-recv-left               pic s9(8) comp value 0.
       77 ws-seq-num                 pic s9(8) comp value 0.
       77 ws-flags                   pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77 WS-COMP-ID                   PIC XXX VALUE 'CID'.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
       01  WS-AM-CITY-STATE            PIC X(30) VALUE SPACES.
       01  ws-work-apr.
           05  ws-apr-num              pic 99V999.
       01  ws-work-ben.
           05  ws-ben-num              pic 9(5).
       01  ws-work-term.
           05  ws-term-num             pic 999.
       01  ws-get-rate-data.
           05  rate-state              pic xx.
           05  rate-lf-ah              pic x.
           05  rate-ben-code           pic xx.
           05  rate-earn-meth          pic x.
           05  rate-ben-amt            pic x(5).
           05  rate-eff-date           pic x(10).
           05  rate-term               pic xxx.
           05  rate-apr                pic x(5).
       01  WS-FIN-RESP                 PIC X(10).
       01  WS-GA-DATA.
           05  WS-GA-NUM               PIC X(10).
           05  WS-GA-PRIMARY-CONTACT   PIC X(30).
           05  WS-GA-NAME              PIC X(30).
           05  WS-GA-MAIL-NAME         PIC X(30).
           05  WS-GA-ADDR1             PIC X(30).
           05  WS-GA-ADDR2             PIC X(30).
           05  WS-GA-ADDR3             PIC X(30).
           05  WS-GA-ZIP               PIC X(9).
           05  WS-GA-PHONE             PIC X(10).

       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).

       01  WS-NT-KEY.
           05  WS-NT-COMPANY-CD        PIC X.                                       
           05  WS-NT-CARRIER           PIC X.                                       
           05  WS-NT-GROUP             PIC X(6).                                    
           05  WS-NT-STATE             PIC XX.   
           05  WS-NT-ACCOUNT           PIC X(10).
           05  WS-NT-REC-TYPE          PIC X.
           05  WS-NT-SEQ-NO            PIC S9(4) COMP.

       01  WS-AM-ALT-KEY.
           05  WS-AM-ALT-ACCOUNT       PIC X(10).
           05  WS-AM-ALT-EXP-DT        PIC XX.

       01  WS-CT-KEY-SAVE              PIC X(5).
       01  WS-CT-KEY.
           05  WS-CT-COMPANY-CD        PIC X.
           05  WS-CT-TABLE             PIC XXX.
           05  WS-CT-BEN-TYPE          PIC X.
           05  WS-CT-BEN-CODE          PIC XX.
           
       01  WS-CF-KEY-SAVE              PIC X(10).
       01  WS-CF-KEY.
           05  WS-CF-COMPANY-ID        PIC XXX.
           05  WS-CF-RECORD-TYPE       PIC X.
           05  WS-CF-ACCESS            PIC X(4).
           05  WS-CF-SEQ-NO            PIC S9(4) COMP.
           
       01  WS-CO-KEY.
           05  WS-CO-COMPANY-CD        PIC X.                                       
           05  WS-CO-CARRIER           PIC X.                                       
           05  WS-CO-GROUP             PIC X(6).                                    
           05  WS-CO-FIN-RESP          PIC X(10).   
           05  WS-CO-ACCOUNT           PIC X(10).
           05  WS-CO-TYPE              PIC X.

       01  WS-WORK-SAH-COMM            PIC SV9(5) VALUE +0.
       01  WS-WORK-JAH-COMM            PIC SV9(5) VALUE +0.
       01  WS-POLICY-NO                PIC X(6).
       01  WS-SHORT-ACCT-NAME          PIC X(30).
       01  WS-LONG-ACCT-NAME           PIC X(60).
       01  WS-CONTR-NAME               PIC X(60).
       01  WS-SL-COMM                  PIC 9.99999.
       01  WS-JL-COMM                  PIC 9.99999.
       01  WS-SAH-COMM                 PIC 9.99999.
       01  WS-JAH-COMM                 PIC 9.99999.
       01  WS-CID-NO                   PIC X(8).

       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.

                                        COPY ERCACNT.
                                        COPY ERCACCT.
                                        COPY ERCCOMP.
                                        COPY ELCCNTL.
                                        COPY ERCCTBL.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       linkage section.
       01  DFHCOMMAREA.
         05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
         05 LSTN-NAME                PIC X(8).
         05 LSTN-SUBNAME             PIC X(8).
         05 CLIENT-IN-DATA.
            15  CLIENT-CAR           PIC X.
            15  CLIENT-GRP           PIC X(6).
            15  CLIENT-ST            PIC XX.
            15  CLIENT-ACT           PIC X(10).
            15  CLIENT-ID            PIC XXX.
            15  FILLER               PIC X(14).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).

       procedure division.
      *
      * when calling a C function the function returns its value
      * in the system variable return code.
      *
       start-here.
      *
      * data with transaction
      *
           display 'SOCK00:transaction data =', CLIENT-IN-DATA.
           display 'SOCK00:socket number    =', GIVE-TAKE-SOCKET.
           IF CLIENT-IN-DATA (1:4) = "RATE"
              SET GET-RATES            TO TRUE
           ELSE
              IF CLIENT-IN-DATA (1:5) = "STATE"
                 SET GET-ACT-ACCTS     TO TRUE
              END-IF
           END-IF

           evaluate client-id
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-SEND-BUF (5:3)
                                          WS-COMP-ID
              when 'AHL'
042612           MOVE X'06'            TO WS-COMP-CD
042612           MOVE 'AHL'            TO WS-SEND-BUF (5:3)
042612                                    WS-COMP-ID
020816        when = 'VPP'
020816           MOVE X'07'            TO WS-COMP-CD
020816           MOVE 'VPP'            TO WS-SEND-BUF (5:3)
020816                                    WS-COMP-ID
062121        when 'FNL'
062121           MOVE X'08'            TO WS-COMP-CD
062121           MOVE 'FNL'            TO WS-SEND-BUF (5:3)
062121                                    WS-COMP-ID
              when other
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-SEND-BUF (5:3)
                                          WS-COMP-ID
           end-evaluate

           move 'CSO'                  to ws-send-buf (1:3)
           move +25                    to ws-send-msg-size

      * send data
      *
           display 'SOCK00:sequence number  =', ws-seq-num.
           display 'SOCK00:send buffer      =', ws-send-buf(1:25).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
               display 'SOCK00:send error ',
               go to socket-error.
      *
      * set up the receive buffer
      *
           move low-values to ws-recv-buf.
           set ws-recv-total to zero.
           compute ws-recv-left = ws-recv-msg-size.
      *
      * receive data
      *

           display 'SOCK00:About to recv '

           call "recv" using by value GIVE-TAKE-SOCKET,
               by reference ws-recv-buf(1+ws-recv-total:ws-recv-left),
               by value ws-recv-left,
               by value ws-flags.
      *
      * test what was received and decide what we should do
      *
           if return-code < zero
              display 'SOCK00:recv error ',
              go to socket-error.

           if return-code = zero
              display 'SOCK00:client disconnected',
              go to socket-error.
      *
           display 'SOCK00:Good recv  '
           display 'SOCK00:return code      = ', return-code
           display 'SOCK00:receive buffer   = ', ws-recv-buf(1:50)

           move +4096                  to ws-send-msg-size
      *    move +750                   to ws-send-msg-size

           if get-rates
              perform 0300-get-rates   thru 0300-exit
           end-if

           IF GET-ACT-ACCTS
              PERFORM 0400-GET-ACT-ACCTS
                                       THRU 0400-EXIT
           END-IF

           move ws-recv-buf (1:19)     to client-in-data
           IF (CLIENT-CAR NOT = ZEROS)
              AND (CLIENT-ST NOT = ZEROS)
              PERFORM 0500-GET-ONE-ACCT
                                       THRU 0500-EXIT
           END-IF

           MOVE ' '                    TO WS-BUILD-SW
           MOVE CLIENT-ACT             TO WS-AM-ALT-ACCOUNT
           MOVE LOW-VALUES             TO WS-AM-ALT-EXP-DT

           PERFORM 0010-STARTBR-ERACCT THRU 0010-EXIT

           IF NOT RESP-NORMAL
              MOVE 'BAD STARTBR ON ERACCT  '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           END-IF

           PERFORM 0020-READNEXT-ERACCT
                                       THRU 0020-EXIT

           IF (NOT RESP-NORMAL)
              OR (AM-ACCOUNT NOT = CLIENT-ACT)
              MOVE 'BAD READNEXT ON ERACCT '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           ELSE
              MOVE AM-EFFECTIVE-DT     TO WS-BIN-ORIG-EFF-DT
           END-IF

           PERFORM UNTIL
              (TIME-TO-BUILD)
              
              IF AM-ACCOUNT = CLIENT-ACT
               IF (AM-STATE = CLIENT-ST)
                       OR
                  (CLIENT-ST = 00)
                 IF AM-EXPIRATION-DT = HIGH-VALUES
                    SET TIME-TO-BUILD  TO TRUE
                 ELSE
                    MOVE ACCOUNT-MASTER
                                       TO WS-SAVE-ERACCT
                    PERFORM 0020-READNEXT-ERACCT
                                       THRU 0020-EXIT
                    IF NOT RESP-NORMAL
                       MOVE WS-SAVE-ERACCT
                                       TO ACCOUNT-MASTER
                       SET TIME-TO-BUILD
                                       TO TRUE
                    END-IF
                 END-IF
               ELSE
                PERFORM 0020-READNEXT-ERACCT
                                  THRU 0020-EXIT
               END-IF
              ELSE
                 MOVE WS-SAVE-ERACCT   TO ACCOUNT-MASTER
                 SET TIME-TO-BUILD     TO TRUE
              END-IF

           END-PERFORM

           PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT

           .
      *
      * send data
      *
       SEND-BUFFER.
       
           display 'SOCK00:About to send      '
           display 'SOCK00:sequence number  =', ws-seq-num.
           display 'SOCK00:send buffer      =', ws-send-buf(1:300).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
               display 'SOCK00:send error ',
               go to socket-error.
           GO TO SOCKET-FIN
      *
      * set up the receive buffer
      *
           move low-values to ws-recv-buf.
           set ws-recv-total to zero.
           compute ws-recv-left = ws-recv-msg-size.
      *
      * receive data
      *
       recv-1.
           call "recv" using by value GIVE-TAKE-SOCKET,
               by reference ws-recv-buf(1+ws-recv-total:ws-recv-left),
               by value ws-recv-left,
               by value ws-flags.
      *
      * test what was received and decide what we should do
      *
           if return-code < zero
              display 'SOCK00:recv error ',
              go to socket-error.

           if return-code = zero
              display 'SOCK00:client disconnected',
              go to socket-error.
      *
      * have we received all the data yet?
      *
           compute ws-recv-total = ws-recv-total + return-code.
           compute ws-recv-left = ws-recv-msg-size - ws-recv-total.
      *
      * not yet
      *
           if ws-recv-left > 0 go to recv-1.
      *
      * received all the data
      *
           display 'SOCK00:receive buffer   =', ws-recv-buf(1:50).
      *
      * make sure what we received was what we sent
      *
      *    if ws-recv-buf <> ws-send-buf
      *        display "SOCK00:data doesn't match",
      *        go to socket-error.
      *
      * end of pass
      *
      *loop-end.
      *    if ws-seq-num = 5 go to socket-error.
      *    go to loop-1.
      *
      * program end
      *
       socket-error.
           if ws-seq-num <> 0
               display "SOCK00:did not complete".
      *
      * flush the send buffer and deallocate
      *
      *    display 'SOCK00:closing socket'.
      *    call "close" using by value GIVE-TAKE-SOCKET .
      *
      * finised return to cics
      *
       socket-fin.
072415*    display 'SOCK00:closing socket'.
072415*    call "close" using by value GIVE-TAKE-SOCKET .
           display 'SOCK00:done'.
           exec cics return end-exec.
           goback.

       0010-STARTBR-ERACCT.
       
           EXEC CICS STARTBR
                DATASET    ('ERACCT3')
                RIDFLD     (WS-AM-ALT-KEY)
                GENERIC
                GTEQ
                KEYLENGTH  (10)
                RESP       (WS-RESPONSE)
           END-EXEC

           .
       0010-EXIT.
           EXIT.
           
       0020-READNEXT-ERACCT.
       
           EXEC CICS READNEXT
                INTO    (ACCOUNT-MASTER)
                DATASET ('ERACCT3')
                RIDFLD  (WS-AM-ALT-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       0020-EXIT.
           EXIT.

       0030-BUILD-BUFFER.
       
           PERFORM 0100-BUILD-ERACCT-STUFF
                                       THRU 0100-EXIT
           MOVE SPACES                 TO WS-SEND-BUF
           EVALUATE AM-STATUS
              WHEN '1'
                 MOVE 'I'              TO WS-STATUS
              WHEN '2'
                 MOVE 'T'              TO WS-STATUS
              WHEN '3'
                 MOVE 'C'              TO WS-STATUS
              WHEN '4'
                 MOVE 'I'              TO WS-STATUS
              WHEN '5'
                 MOVE 'S'              TO WS-STATUS
021916        WHEN '6'
021916           MOVE 'D'              TO WS-STATUS
021916        WHEN '7'
021916           MOVE 'L'              TO WS-STATUS
021916        WHEN '8'
021916           MOVE 'R'              TO WS-STATUS
021916        WHEN '9'
021916           MOVE 'P'              TO WS-STATUS
              WHEN OTHER
                 MOVE 'A'              TO WS-STATUS
           END-EVALUATE

           IF AM-REMIT-TO NOT NUMERIC
              MOVE ZEROS         TO AM-REMIT-TO
           END-IF
           IF AM-REMIT-TO = ZEROS
              MOVE 01        TO AM-REMIT-TO
           END-IF
           MOVE AM-AGT (AM-REMIT-TO) TO WS-FIN-RESP
           MOVE SPACES                 TO WS-AM-CITY-STATE
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO WS-AM-CITY-STATE
           END-STRING
           IF NOT GET-ACT-ACCTS
      * this is when a single account is selected
             STRING
              AM-ACCOUNT      ';'
              WS-LONG-ACCT-NAME    ';' 
              WS-CONTR-NAME   ';'
              AM-ADDRS        ';'
              WS-AM-CITY-STATE ';'
              AM-ZIP          ';'
              AM-GPCD         ';'
              WS-EFF-DATE     ';'
              WS-EXP-DATE     ';'
              WS-POLICY-NO    ';'
              WS-SL-COMM      ';'
              WS-JL-COMM      ';'
              WS-SAH-COMM     ';'
              WS-JAH-COMM     ';'
              WS-CID-NO       ';'
              AM-STATE        ';'
              WS-GA-NUM       ';'
              WS-GA-PRIMARY-CONTACT ';'
              WS-GA-NAME      ';'
              WS-GA-MAIL-NAME ';'
              WS-GA-ADDR1     ';'
              WS-GA-ADDR2     ';'
              WS-GA-ADDR3     ';'
              WS-GA-ZIP       ';'
              WS-GA-PHONE     ';'
              AM-CONTROL-NAME ';'
              WS-ORIG-EFF-DT  ';'
              AM-COMMENT-LINE (1) ';'
              AM-COMMENT-LINE (2) ';'
              AM-COMMENT-LINE (3) ';'
              AM-COMMENT-LINE (4) ';'
              AM-COMMENT-LINE (5) ';'
              AM-TEL-NO       ';'
              AM-REPORT-CODE-1    ';'
              AM-REPORT-CODE-2    ';'
              WS-STATUS           ';'
              AM-REPORT-CODE-3    ';'
              AM-USER-SELECT-2    ';'
              AM-CARRIER          ';'
              WS-FIN-RESP         ';'
              AM-USER-SELECT-3    ';'
              WS-SHORT-ACCT-NAME  ';'
                 DELIMITED BY SIZE INTO WS-SEND-BUF
             END-STRING
           ELSE
             STRING
              AM-ACCOUNT      ';'
              WS-LONG-ACCT-NAME    ';' 
              WS-CONTR-NAME   ';'
              AM-ADDRS        ';'
              WS-AM-CITY-STATE ';'
              AM-ZIP          ';'
              AM-GPCD         ';'
              WS-EFF-DATE     ';'
              WS-EXP-DATE     ';'
              WS-POLICY-NO    ';'
              WS-SL-COMM      ';'
              WS-JL-COMM      ';'
              WS-SAH-COMM     ';'
              WS-JAH-COMM     ';'
              WS-CID-NO       ';'
              AM-STATE        ';'
              WS-GA-NUM       ';'
              WS-GA-PRIMARY-CONTACT ';'
              WS-GA-NAME      ';'
              WS-GA-MAIL-NAME ';'
              WS-GA-ADDR1     ';'
              WS-GA-ADDR2     ';'
              WS-GA-ADDR3     ';'
              WS-GA-ZIP       ';'
              WS-GA-PHONE     ';'
              AM-COMMENT-LINE (1) ';'
              AM-COMMENT-LINE (2) ';'
              AM-COMMENT-LINE (3) ';'
              AM-COMMENT-LINE (4) ';'
              AM-COMMENT-LINE (5) ';'
              AM-CONTROL-NAME     ';'
              WS-ORIG-EFF-DT      ';'
              AM-TEL-NO           ';'
              AM-REPORT-CODE-1    ';'
              AM-REPORT-CODE-2    ';'
              WS-STATUS           ';'
              AM-REPORT-CODE-3    ';'
              AM-USER-SELECT-2    ';'
              AM-CARRIER          ';'
              WS-FIN-RESP         ';'
              AM-USER-SELECT-3    ';'
              WS-SHORT-ACCT-NAME  ';'
                 DELIMITED BY SIZE INTO WS-SEND-BUF
             END-STRING
           END-IF
                 
           .
       0030-EXIT.
           EXIT.
           
       0100-BUILD-ERACCT-STUFF.
       
           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-EFF-DATE
           ELSE
              MOVE '00/00/0000'        TO WS-EFF-DATE
           END-IF
           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '99/99/9999'        TO WS-EXP-DATE
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK   THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS-EXP-DATE
              ELSE
                 MOVE '00/00/0000'     TO WS-EXP-DATE
              END-IF
           END-IF
           MOVE WS-BIN-ORIG-EFF-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-ORIG-EFF-DT
           ELSE
              MOVE '00/00/0000'        TO WS-ORIG-EFF-DT
           END-IF
           MOVE AM-COMMENT-LINE (2)    TO WS-POLICY-NO
           PERFORM VARYING S1 FROM +7 BY +1 UNTIL
              (S1 > +48)
              OR (AM-COMMENT-LINE (2) (S1:3) = 'CID')
           END-PERFORM
           IF S1 > +48
              MOVE SPACES              TO WS-CID-NO
           ELSE
              MOVE AM-COMMENT-LINE (2) (S1:8)
                                       TO WS-CID-NO
           END-IF
           MOVE AM-NAME                TO WS-SHORT-ACCT-NAME
                                          WS-LONG-ACCT-NAME
           MOVE AM-PERSON              TO WS-CONTR-NAME

      *    IF AM-REPORT-CODE-3 = 'RLIC'
              MOVE AM-CONTROL-PRIMARY  TO WS-NT-KEY
              MOVE '3'                 TO WS-NT-REC-TYPE
              MOVE +1                  TO WS-NT-SEQ-NO
              EXEC CICS READ
                 DATASET    ('ERACNT')
                 RIDFLD     (WS-NT-KEY)
                 INTO       (NOTE-FILE)
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 IF NT-SHIPPING-LINE NOT = SPACES AND LOW-VALUES
                    MOVE NT-SHIPPING-LINE TO WS-LONG-ACCT-NAME
                 END-IF
              END-IF

              MOVE AM-CONTROL-PRIMARY  TO WS-NT-KEY
              MOVE '3'                 TO WS-NT-REC-TYPE
              MOVE +2                  TO WS-NT-SEQ-NO
              EXEC CICS READ
                 DATASET    ('ERACNT')
                 RIDFLD     (WS-NT-KEY)
                 INTO       (NOTE-FILE)
                 RESP       (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 IF NT-SHIPPING-LINE NOT = SPACES AND LOW-VALUES
                    MOVE NT-SHIPPING-LINE TO WS-CONTR-NAME
                 END-IF
              END-IF
      *    END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +10)
              OR (AM-COM-TYP (S1) = 'D' OR 'C')
           END-PERFORM
           IF S1 > +10
              CONTINUE
           ELSE
              PERFORM 0150-BUILD-DW-COMM
                                       THRU 0150-EXIT
           END-IF
           MOVE SPACES                 TO WS-GA-DATA
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +10)
052814        OR (AM-COM-TYP (S1) = 'O' OR 'P' OR 'K' OR 'S')
           END-PERFORM
           IF S1 > +10
              MOVE ZEROS               TO WS-GA-NUM
           ELSE
              MOVE AM-AGT (S1)         TO WS-GA-NUM
           END-IF
           IF WS-GA-NUM NOT = ZEROS
              PERFORM 0200-GET-ERCOMP  THRU 0200-EXIT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0150-BUILD-DW-COMM.
       
           MOVE ZEROS                  TO WS-SL-COMM
                                          WS-JL-COMM
                                          WS-SAH-COMM
                                          WS-JAH-COMM
                                          WS-WORK-SAH-COMM
                                          WS-WORK-JAH-COMM
                                          
           IF (AM-L-COM (S1) NOT NUMERIC)
              OR (AM-L-COMA (S1) (3:1) = 'L' OR 'M')
              CONTINUE
           ELSE
              MOVE AM-L-COM (S1)       TO WS-SL-COMM
           END-IF

           IF (AM-J-COM (S1) NOT NUMERIC)
              OR (AM-J-COMA (S1) (3:1) = 'L' OR 'M')
              CONTINUE
           ELSE
              MOVE AM-J-COM (S1)       TO WS-JL-COMM
           END-IF

           IF (AM-A-COM (S1) NOT NUMERIC)
              OR (AM-A-COMA (S1) (3:1) = 'L' OR 'M')
              PERFORM 0160-ERCTBL-STARTBR
                                       THRU 0160-EXIT
              IF RESP-NORMAL
                 PERFORM 0165-ERCTBL-READNEXT
                                       THRU 0165-EXIT
                 UNTIL
                 ((WS-WORK-SAH-COMM NOT = ZEROS)
                  AND (WS-WORK-JAH-COMM NOT = ZEROS))
                             OR
                  END-OF-ERCTBL
                  MOVE WS-WORK-JAH-COMM TO WS-JAH-COMM
                  MOVE WS-WORK-SAH-COMM TO WS-SAH-COMM
              END-IF
           ELSE
              MOVE AM-A-COM (S1)       TO WS-SAH-COMM
                                          WS-JAH-COMM
           END-IF

           .
       0150-EXIT.
           EXIT.

       0160-ERCTBL-STARTBR.

           MOVE AM-COMPANY-CD          TO WS-CT-COMPANY-CD
           MOVE AM-A-COMA (S1)         TO WS-CT-TABLE
           MOVE 'A'                    TO WS-CT-BEN-TYPE
           MOVE LOW-VALUES             TO WS-CT-BEN-CODE
           MOVE WS-CT-KEY              TO WS-CT-KEY-SAVE

           EXEC CICS STARTBR
                DATASET    ('ERCTBL')
                RIDFLD     (WS-CT-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           .
       0160-EXIT.
           EXIT.

       0165-ERCTBL-READNEXT.
       
           EXEC CICS READNEXT
                INTO    (COMM-TABLE-RECORD)
                DATASET ('ERCTBL')
                RIDFLD  (WS-CT-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           IF (RESP-NORMAL)
              AND (WS-CT-KEY (1:5) = WS-CT-KEY-SAVE (1:5))
              PERFORM 0170-PROCESS-BEN-CODE
                                       THRU 0170-EXIT
           ELSE
              SET END-OF-ERCTBL        TO TRUE
           END-IF

           .
       0165-EXIT.
           EXIT.


       0170-PROCESS-BEN-CODE.

           MOVE SPACES                 TO WS-CF-KEY
           MOVE WS-COMP-ID             TO WS-CF-COMPANY-ID
           MOVE '5'                    TO WS-CF-RECORD-TYPE
           MOVE CT-BEN-CODE            TO WS-CF-ACCESS (3:2)
           MOVE +0                     TO WS-CF-SEQ-NO
           MOVE WS-CF-KEY              TO WS-CF-KEY-SAVE
           
           EXEC CICS READ
                DATASET    ('ELCNTL')
                RIDFLD     (WS-CF-KEY)
                INTO       (CONTROL-FILE)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE WS-RESPONSE         TO WS-DISP-RESP
              DISPLAY ' ERROR - ELCNTL - READ ' WS-DISP-RESP
              GO TO 0170-EXIT
           END-IF
           
           IF WS-CF-KEY (1:4) = WS-CF-KEY-SAVE (1:4)
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 (S2 > +8)
                 OR (CT-BEN-CODE = CF-BENEFIT-CODE (S2))
              END-PERFORM
              IF CT-BEN-CODE = CF-BENEFIT-CODE (S2)
                 IF CF-JOINT-INDICATOR (S2) = 'J'
                    IF WS-WORK-JAH-COMM = ZEROS
                       MOVE CT-RT (1)  TO WS-WORK-JAH-COMM
                    END-IF
                 ELSE
                    IF WS-WORK-SAH-COMM = ZEROS
                       MOVE CT-RT (1)  TO WS-WORK-SAH-COMM
                    END-IF
                 END-IF
              END-IF
           END-IF
                    
                 
           .
       0170-EXIT.
           EXIT.

       0200-GET-ERCOMP.

           MOVE LOW-VALUES             TO WS-CO-KEY
           MOVE AM-COMPANY-CD          TO WS-CO-COMPANY-CD
           MOVE AM-CARRIER             TO WS-CO-CARRIER
           MOVE AM-GROUPING            TO WS-CO-GROUP
           MOVE WS-GA-NUM              TO WS-CO-FIN-RESP
           MOVE LOW-VALUES             TO WS-CO-ACCOUNT
           MOVE 'G'                    TO WS-CO-TYPE

           EXEC CICS READ
                INTO    (COMPENSATION-MASTER)
                DATASET ('ERCOMP')
                RIDFLD  (WS-CO-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE CO-CONTROL-NAME     TO WS-GA-PRIMARY-CONTACT
              MOVE CO-ACCT-NAME        TO WS-GA-NAME
              MOVE CO-MAIL-NAME        TO WS-GA-MAIL-NAME
              MOVE CO-ADDR-1           TO WS-GA-ADDR1
              MOVE CO-ADDR-2           TO WS-GA-ADDR2
      *       MOVE CO-ADDR-3           TO WS-GA-ADDR3
              MOVE SPACES              TO WS-GA-ADDR3
              STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                 DELIMITED BY '  ' INTO WS-GA-ADDR3
              END-STRING
              MOVE CO-ZIP              TO WS-GA-ZIP
              MOVE CO-TELEPHONE        TO WS-GA-PHONE
           ELSE
              MOVE SPACES              TO WS-GA-DATA
              MOVE 'GA NOT FOUND '     TO WS-GA-NAME
           END-IF
           

           .
       0200-EXIT.
           EXIT.

       0300-get-rates.

           move +30                    to ws-send-msg-size

           move ws-recv-buf (1:31)     to ws-get-rate-data

           if rate-apr not numeric
              move zeros               to rate-apr
           end-if
           move rate-apr               to ws-work-apr

           if rate-ben-amt not numeric
              move zeros               to rate-ben-amt
           end-if
           move rate-ben-amt           to ws-work-ben

           if rate-term not numeric
              move zeros               to rate-term
           end-if
           move rate-term              to ws-work-term
           
           move rate-eff-date (7:4)    to ws-work-date (1:4)
           move rate-eff-date (1:2)    to ws-work-date (5:2)
           move rate-eff-date (4:2)    to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-dt
           else
              move 'bad eff dt convert ' to ws-send-buf
              go to send-rate-buf
           end-if
           
           move +1                     to dc-elapsed-months
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-1st-pay-dt
           else
              move 'bad 1st dt convert ' to ws-send-buf
              go to send-rate-buf
           end-if
           
      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-class-code
                                          cp-deviation-code
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
           move rate-state             to cp-state
                                          cp-state-std-abbrv
           move rate-lf-ah             to cp-benefit-type
           move rate-ben-code          to cp-benefit-cd
           move ws-ben-num             to cp-original-benefit
                                          cp-rating-benefit-amt
           move 40                     to cp-issue-age
           move WS-COMP-ID             to cp-company-id
           move WS-COMP-CD             to cp-company-cd

           move ws-apr-num             to cp-loan-apr

           move ws-term-num            to cp-original-term
                                          cp-loan-term
                                          cp-remaining-term

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move rate-earn-meth         to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pay-dt      to cp-first-pay-date
           
           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           if not no-cp-error
              move cp-return-code      to ws-send-buf
           else
              move cp-calc-premium     to ws-disp-amt
              move cp-premium-rate     to ws-disp-rate
              string ws-disp-amt ';' ws-disp-rate delimited
                 by size into ws-send-buf
              end-string
           end-if
           .
       SEND-RATE-BUF.
       
           display 'SOCK00:About to send      '
           display 'SOCK00:sequence number  =', ws-seq-num.
           display 'SOCK00:send buffer      =', ws-send-buf(1:30).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
               display 'SOCK00:send error ',
               go to socket-error.
           GO TO SOCKET-FIN

           .
       0300-exit.
           exit.
           
       0400-GET-ACT-ACCTS.

           MOVE WS-RECV-BUF (1:2)      TO CLIENT-ST
           MOVE ' '                    TO WS-BUILD-SW
           MOVE LOW-VALUES             TO WS-AM-KEY
           MOVE WS-COMP-CD             TO WS-AM-COMPANY-CD

           PERFORM 0410-STARTBR-ERACCT THRU 0410-EXIT

           IF NOT RESP-NORMAL
              MOVE 'BAD STARTBR ON ERACCT  '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           END-IF

           PERFORM 0420-READNEXT-ERACCT
                                       THRU 0420-EXIT

           IF NOT RESP-NORMAL
              MOVE 'BAD READNEXT ON ERACCT '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           ELSE
              MOVE AM-EFFECTIVE-DT     TO WS-BIN-ORIG-EFF-DT
              MOVE AM-ACCOUNT          TO WS-SAVE-ACCOUNT
           END-IF

           PERFORM UNTIL END-OF-ERACCT
              IF NOT (RESP-NORMAL)
                 OR (AM-COMPANY-CD > WS-COMP-CD)
                 SET END-OF-ERACCT     TO TRUE
              ELSE
                 IF AM-ACCOUNT NOT = WS-SAVE-ACCOUNT
                    MOVE AM-EFFECTIVE-DT TO WS-BIN-ORIG-EFF-DT
                 END-IF                 
                 IF (AM-STATE = CLIENT-ST)
                    AND (AM-EXPIRATION-DT = HIGH-VALUES)
      *             AND (AM-STATUS = '0')
                    PERFORM 0030-BUILD-BUFFER
                                       THRU 0030-EXIT
                    PERFORM 0430-SEND-BUFFER
                                       THRU 0430-EXIT
                 END-IF
                 PERFORM 0420-READNEXT-ERACCT
                                       THRU 0420-EXIT
              END-IF
           END-PERFORM

           GO TO SOCKET-FIN

           .
       0400-EXIT.
           EXIT.
           
       0410-STARTBR-ERACCT.
       
           EXEC CICS STARTBR
                DATASET    ('ERACCT')
                RIDFLD     (WS-AM-KEY)
                GTEQ
                RESP       (WS-RESPONSE)
           END-EXEC

           .
       0410-EXIT.
           EXIT.
           
       0420-READNEXT-ERACCT.
       
           EXEC CICS READNEXT
                INTO    (ACCOUNT-MASTER)
                DATASET ('ERACCT')
                RIDFLD  (WS-AM-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       0420-EXIT.
           EXIT.

       0430-SEND-BUFFER.

           display 'SOCK00:About to send      '
           display 'SOCK00:sequence number  =', ws-seq-num.
           display 'SOCK00:send buffer      =', ws-send-buf(1:300).

           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.

           if return-code <= zero
               display 'SOCK00:send error ',
               go to socket-error.

           .
       0430-EXIT.
           EXIT.
           

       0500-GET-ONE-ACCT.

           MOVE WS-COMP-CD             TO WS-AM-COMPANY-CD
           MOVE CLIENT-CAR             TO WS-AM-CARRIER
           MOVE CLIENT-GRP             TO WS-AM-GROUP
           MOVE CLIENT-ST              TO WS-AM-STATE
           MOVE CLIENT-ACT             TO WS-AM-ACCOUNT

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-BIN-DATE-1          TO WS-AM-EXP-DT
           MOVE LOW-VALUES             TO WS-AM-EXP-DT
           MOVE ' '                    TO WS-BUILD-SW

          PERFORM 0410-STARTBR-ERACCT THRU 0410-EXIT

           IF NOT RESP-NORMAL
              MOVE 'BAD STARTBR ON ERACCT  '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           END-IF

           PERFORM 0420-READNEXT-ERACCT
                                       THRU 0420-EXIT

           IF NOT RESP-NORMAL
              MOVE 'BAD READNEXT ON ERACCT '
                                       TO WS-SEND-BUF
              MOVE WS-RESPONSE         TO WS-DIS-RESP
              MOVE WS-DIS-RESP         TO WS-SEND-BUF (25:5)
              GO TO SEND-BUFFER
           ELSE
              MOVE AM-EFFECTIVE-DT     TO WS-BIN-ORIG-EFF-DT
              MOVE AM-ACCOUNT          TO WS-SAVE-ACCOUNT
           END-IF

           PERFORM UNTIL
              (TIME-TO-BUILD)
              
              IF AM-ACCOUNT = CLIENT-ACT
                 IF AM-EXPIRATION-DT = HIGH-VALUES
                    SET TIME-TO-BUILD  TO TRUE
                 ELSE
                    MOVE ACCOUNT-MASTER
                                       TO WS-SAVE-ERACCT
                    PERFORM 0420-READNEXT-ERACCT
                                       THRU 0420-EXIT
                    IF NOT RESP-NORMAL
                       MOVE WS-SAVE-ERACCT
                                       TO ACCOUNT-MASTER
                       SET TIME-TO-BUILD
                                       TO TRUE
                    END-IF
                 END-IF
              ELSE
                 MOVE WS-SAVE-ERACCT   TO ACCOUNT-MASTER
                 SET TIME-TO-BUILD     TO TRUE
              END-IF

           END-PERFORM

           PERFORM 0030-BUILD-BUFFER   THRU 0030-EXIT

           PERFORM 0430-SEND-BUFFER    THRU 0430-EXIT

           GO TO SOCKET-FIN

           .
       0500-EXIT.
           EXIT.
           
       7000-GET-RATE.
       
           EXEC CICS LINK
              PROGRAM ('ELRATE')
              COMMAREA (CALCULATION-PASS-AREA)
              LENGTH   (CP-COMM-LENGTH)
           END-EXEC
           
           .
       7000-EXIT.
           EXIT.
           
       9700-DATE-LINK.

           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.

       9700-EXIT.
            EXIT.

