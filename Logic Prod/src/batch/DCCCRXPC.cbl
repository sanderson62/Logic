       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCCRXPC.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *  DESCRIPTION:
      *      THIS PROGRAM EXTRACTS RECORDS IDENTIFYING CERTS REQUIRING
      *      POSTCARDS TO BE PRINTED.  TWO TYPES OF POSTCARDS ARE
      *      HANDLED: FIRST YEAR ANNIVERSARY; AND AT EXPIRATION DATE.
      *
      *    DATE    BY  MODIFICATION
      * ========== === ================================================
022208* 02/22/2008 SDG REVISED HANDLING FOR CERTS WITH ONE CANCELLED 
022208*                COVERAGE.  SEE CR #2008022100002.
061813* 06/18/2013 PEM CR2013061000001 - ADD "CATCH-UP" PROCESSING
121213* 12/12/2013 PEM CR2011042100001 - NEW PROGRAM CIDCRXPC
      *::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-FILE-IN     ASSIGN TO CERTIN.

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED              
                                   FILE STATUS IS ERMAIL-FILE-STATUS   
                                   RECORD KEY IS MA-CONTROL-PRIMARY.    

           SELECT ELMSTR           ASSIGN TO ELMSTR5
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-BY-CERT-NO
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

      ***********    Expires   **********
           SELECT EXTR-FILE-OUT1   ASSIGN TO EXTROT1
               ORGANIZATION IS LINE SEQUENTIAL.

      ***********    Anniv and Catch-up   **********
           SELECT EXTR-FILE-OUT2   ASSIGN TO EXTROT2
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  CERT-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  ERMAIL.                                                      
                                       COPY ERCMAIL.

       FD  ELMSTR.
                                       COPY ELCMSTR.

       FD  EXTR-FILE-OUT1
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-HEAD1         PIC X(367).
       01  EXTR-FILE-OUT-REC1          PIC X(320).

       FD  EXTR-FILE-OUT2
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-HEAD2         PIC X(367).
       01  EXTR-FILE-OUT-REC2          PIC X(320).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '  DCCCRXPC  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

061813 77  m1                      pic s999  comp-3 value +0.
061813 77  ws-pc-sw                pic x   value spaces.
061813     88  pc-sent                  value 'Y'.
       77  WS-SAVE-ELMSTR-KEY      PIC X(12)  VALUE SPACES.
       77  WS-CLAIM-SW             PIC X   VALUE ' '.
           88  CLAIM-FOUND                 VALUE 'Y'.
       77  WS-CLAIM-OPEN-SW        PIC X   VALUE ' '.
           88  CLAIM-OPEN                  VALUE 'Y'.
       77  WS-DTH-CLAIM-SW         PIC X   VALUE ' '.
           88  DTH-CLAIM-FOUND             VALUE 'Y'.
       77  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  ELMSTR-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  ERMAIL-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-CERT               VALUE 'Y'.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT1           PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT2           PIC 9(9) VALUE ZEROS.
031109 77  EXT-RECS-OUT3           PIC 9(9) VALUE ZEROS.
061813 77  EXT-RECS-OUT4           PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EXPIRE-DT            PIC 9(11)   VALUE ZEROS.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EARN-TERM            PIC 999   VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.
       77  WS-LAST-MONTH-DT        PIC 9(8)  VALUE ZEROS.
       77  WS-12-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
       77  WS-13-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
031109 77  WS-CERT-DT              PIC 9(8)  VALUE ZEROS.
031109 77  WS-RUN-DT               PIC 9(8)  VALUE ZEROS.
       77  WS-BAD-W-CLM            PIC 9(9)  VALUE ZEROS.
       77  ws-pd-thru-dt           pic xx    value low-values.
       77  WS-TYPE-EXTRACT         PIC X     VALUE SPACES.
           88  WS-EXPIRE                     VALUE 'E'.
           88  WS-ANNIV                      VALUE 'A'.
061813     88  ws-catchup                    VALUE 'C'.
       01  WS-BUS-TYPE                 PIC 99.
           88  WS-AUTO                     VALUE 01.
           88  WS-MARINE                   VALUE 06.
           88  WS-TV                       VALUE 07.
           88  WS-MUSIC                    VALUE 08.
           88  WS-MOBILE-HOME              VALUE 09.
           88  WS-HOME-IMPR                VALUE 10.
           88  WS-SWIMMING-POOL            VALUE 11.
           88  WS-USED-CARS                VALUE 14.
           88  WS-MOTORCYCLE               VALUE 15.
           88  WS-RECREATION-VEH           VALUE 18.
           88  WS-FURNITURE                VALUE 19.
           88  WS-DESIRED-GROUP            VALUE 01 06 07 08 09
                                              10 11 14 15 18 19.
       01  EXTR-DETAIL-RECORD.
           12  EX-FNAME                PIC X(10).
           12  EX-TAB1                 PIC X.
           12  EX-MID                  PIC X.
           12  EX-TAB2                 PIC X.
           12  EX-LNAME                PIC X(15).
           12  EX-TAB3                 PIC X.
           12  EX-ADDRESS1             PIC X(30).
           12  EX-TAB4                 PIC X.
           12  EX-ADDRESS2             PIC X(30).
           12  EX-TAB5                 PIC X.
           12  EX-CITY-STATE           PIC X(32).
           12  EX-TAB6                 PIC X.
           12  EX-ZIP                  PIC X(11).
           12  EX-TAB7                 PIC X.
           12  EX-CERT                 PIC X(11).
           12  EX-TAB8                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-EXP                  PIC X(10).
           12  EX-TAB10                PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB11                PIC X.
           12  EX-ACCOUNT-NAME         PIC X(30).
           12  EX-TAB12                PIC X.
           12  EX-CARRIER-GRP-ST       PIC X(9).
           12  EX-TAB13                PIC X.
           12  EX-CLM-IND              PIC X.
           12  EX-TAB14                PIC X.
061813     12  ex-catch-up             pic x.
061813     12  ex-tab15                pic x.
           12  EX-END                  PIC X.

      ******************************************************************
       01  EXTR-DETAIL-HEADER.
           12  FILLER                  PIC X(10) VALUE
                                               'FIRST NAME'.
           12  EX-HTAB1                PIC X.
           12  FILLER                  PIC X(03) VALUE
                                               'MID'.
           12  EX-HTAB2                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LAST NAME'.
           12  EX-HTAB3                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ADDRESS 1'.
           12  EX-HTAB4                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ADDRESS 2'.
           12  EX-HTAB5                PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'CITY STATE'.
           12  EX-HTAB6                PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'ZIP CODE'.
           12  EX-HTAB7                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'CERT NO'.
           12  EX-HTAB8                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'EFF DTE'.
           12  EX-HTAB9                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'EXP DTE'.
           12  EX-HTAB10               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'ACCT NO'.
           12  EX-HTAB11               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ACCT NAME'.
           12  EX-HTAB12               PIC X.
           12  FILLER                  PIC X(8)  VALUE
                                                'CARGRPST'.
           12  EX-HTAB13               PIC X.
           12  FILLER                  PIC X(6)  VALUE
                                                 'CLMIND'.
           12  EX-HTAB14               PIC X.
061813     12  filler                  pic x(7)  value 'CATCHUP'.
061813     12  ex-htab15               pic x.
           12  FILLER                  PIC X(03) VALUE
                                               'EOR'.
      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-EXTR            PIC X(319) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-CERT)
PEMTST*       OR (CRT-RECS-IN > 50000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ     '  CRT-RECS-IN
           DISPLAY ' EXPIRED THIS MONTH    '  EXT-RECS-OUT1
           DISPLAY ' EFFECTIVE 12 MOS AGO  '  EXT-RECS-OUT2
031109     DISPLAY ' SPECIAL ANNIVERSARY   '  EXT-RECS-OUT3
061813     display ' Catch up records      '  ext-recs-out4
           DISPLAY '  BAD ADDRESS WITH CLM '  WS-BAD-W-CLM
           GOBACK

           .
       0050-PROCESS.
       
022208     MOVE ZERO                   TO WS-EXPIRE-DT
031109     MOVE SPACE                  TO WS-TYPE-EXTRACT

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  We are only interested in addendums where at least one    ***
      ***  coverage is active and there has not been a death claim.  ***
      ***  Also, we are excluding any DCC addendums with effective   ***
      ***  dates prior to 07/01/2011 when the product started.       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

022208     IF ZEROS = CR-LF-CANCEL-EXIT-DATE
022208              AND  CR-LF-CLAIM-EXIT-DATE
              MOVE CR-LF-EXPIRE-DATE   TO WS-EXPIRE-DT
022208     END-IF
 
022208     IF ZEROS = CR-AH-CANCEL-EXIT-DATE
022208              AND  CR-AH-SETTLEMENT-EXIT-DATE
              IF CR-AH-EXPIRE-DATE > WS-EXPIRE-DT
                 MOVE CR-AH-EXPIRE-DATE TO WS-EXPIRE-DT
              END-IF
022208     END-IF
           
031109     IF CR-DTH-DT > ZEROS
031109        MOVE ZEROS               TO WS-EXPIRE-DT
031109     END-IF
           
022208     IF (WS-EXPIRE-DT > ZEROS)
              and (cr-dt > 20110630)
              IF (WS-EXPIRE-DT > WS-LAST-MONTH-DT)
                 AND (WS-EXPIRE-DT <= RUN-DATE)
                 AND (CR-ENTRY-STATUS NOT = 'D' AND 'V' AND '9' AND '5')
                 SET WS-EXPIRE         TO TRUE
                 PERFORM 0100-PROCESS-CERT
                                       THRU 0100-EXIT
              END-IF
              IF (CR-DT > WS-13-MONTHS-AGO)
                 AND (CR-DT <= WS-12-MONTHS-AGO)
                 AND ((CR-LF-TERM > 24)
                           OR
                     (CR-AH-TERM > 24))
                 AND (CR-ENTRY-STATUS NOT = 'D' AND 'V' AND '9' AND '5')
031109           AND WS-TYPE-EXTRACT = SPACE
                 SET WS-ANNIV          TO TRUE
                 PERFORM 0100-PROCESS-CERT
                                       THRU 0100-EXIT
              END-IF
061813        if (cr-entry-date = run-date)
061813           if  (CR-DT <= WS-12-MONTHS-AGO)
061813              AND ((CR-LF-TERM > 24)
061813                     OR
061813               (CR-AH-TERM > 24))
061813              AND (CR-ENTRY-STATUS NOT = 'D' AND 'V'
061813                                AND '9' AND '5')
061813              AND (WS-TYPE-EXTRACT = SPACE)
061813              perform 0115-check-for-post-card
061813                                 thru 0115-exit
061813              if not pc-sent
061813                 SET WS-catchup  TO TRUE
061813                 PERFORM 0100-PROCESS-CERT
061813                                 THRU 0100-EXIT
061813              end-if
061813           end-if
061813        END-IF
           END-IF

           PERFORM 0280-READ-CERT      THRU 0280-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-CERT.

           PERFORM 0175-SYNC-CERT-ERACCT
                                       THRU 0175-EXIT

           MOVE AM-GPCD                TO WS-BUS-TYPE
           IF WS-DESIRED-GROUP
              MOVE WS-SAVE-EXTR        TO EXTR-DETAIL-RECORD

              MOVE CR-CERT-NO          TO EX-CERT
              MOVE CR-DT               TO WS-DATE
                                          DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-EFF-BIN
              ELSE
                 DISPLAY ' DATE CONVERT ERROR, EFF DATE ' CR-DT
              END-IF
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EX-EFF
              END-STRING

              MOVE WS-EXPIRE-DT        TO WS-DATE
                                          DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-EXP-BIN
              ELSE
                 DISPLAY ' DATE CONVERT ERROR, EXP DATE ' CR-DT
              END-IF

              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EX-EXP
              END-STRING
           
              MOVE CR-ACCOUNT          TO EX-ACCOUNT
              MOVE AM-NAME             TO EX-ACCOUNT-NAME
      *       MOVE AM-GPCD             TO EX-ACCOUNT-NAME (29:2)
           
              MOVE CR-LNAME            TO EX-LNAME
              MOVE CR-FNAME            TO EX-FNAME
              MOVE CR-INIT             TO EX-MID
              STRING CR-CARRIER CR-GROUPING CR-STATE DELIMITED BY SIZE
                 INTO EX-CARRIER-GRP-ST
              END-STRING
              PERFORM 0200-PROCESS-ELMSTR
                                       THRU 0200-EXIT
              IF CLAIM-FOUND
                 IF DTH-CLAIM-FOUND
                    MOVE 'D'           TO EX-CLM-IND
                 ELSE
                    IF CLAIM-OPEN
                       MOVE 'O'        TO EX-CLM-IND
                    ELSE
                       MOVE 'C'        TO EX-CLM-IND
                    END-IF
                 END-IF
              ELSE
                 MOVE 'N'              TO EX-CLM-IND
              END-IF
              
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  If we find a Life claim, bypass. If we find an open A&H   ***
      ***  claim, bypass. If we find a closed claim on an expire     ***
      ***  where the paid thru date is >= the expire date then       ***
      ***  bypass.                                                   ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

              evaluate true
                 when ex-clm-ind = 'D'
                    DISPLAY ' bypassing due to DTH CLAIM '
                       cr-carrier ' ' cr-state ' ' cr-account
                       ' ' cr-dt ' ' CR-CERT-NO
                    go to 0100-exit
                 when ex-clm-ind = 'O'
                    DISPLAY ' bypassing due to open CLAIM '
                       cr-carrier ' ' cr-state ' ' cr-account
                       ' ' cr-dt ' ' CR-CERT-NO
                    go to 0100-exit
                 when ex-clm-ind = 'C'
                    if (ws-expire)
                       and (ws-pd-thru-dt >= ws-exp-bin)
                       DISPLAY ' bypassing due to CLAIM PD THRU EXPIRY '
                          cr-carrier ' ' cr-state ' ' cr-account
                          ' ' cr-dt ' ' CR-CERT-NO
                       go to 0100-exit
                    end-if
              end-evaluate

              PERFORM 0110-GET-ERMAIL  THRU 0110-EXIT
           
              IF ERMAIL-FILE-STATUS = '00'
                 MOVE MA-ADDRESS-LINE-1
                                       TO EX-ADDRESS1
                 MOVE MA-ADDRESS-LINE-2
                                       TO EX-ADDRESS2
                 MOVE MA-CITY-STATE    TO EX-CITY-STATE
                 MOVE MA-ZIP           TO EX-ZIP
              END-IF
              PERFORM 0300-WRITE-EXTR  THRU 0300-EXIT
           END-IF           

           .
       0100-EXIT.
           EXIT.
           
       0110-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
           MOVE CR-CARRIER             TO MA-CARRIER
           MOVE CR-GROUPING            TO MA-GROUPING
           MOVE CR-STATE               TO MA-STATE
           MOVE CR-ACCOUNT             TO MA-ACCOUNT
           MOVE CR-CERT-NO             TO MA-CERT-NO
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO MA-CERT-EFF-DT
              READ ERMAIL
              IF ERMAIL-FILE-STATUS = '00' OR '23' OR '22' OR '10'
                 CONTINUE
              ELSE
                 DISPLAY ' ERMAIL ERROR - READ ' ERMAIL-FILE-STATUS
                    '  ' CR-CERT-NO
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0110-EXIT.
           EXIT.

061813 0115-check-for-post-card.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  We perform this routine to determine if a post card was   ***
      ***  previously processed. Also, if the cert was re-issued for ***
      ***  some reason with a suffix, we check the cert without a    ***
      ***  suffix to determine if a post card was attempted.         ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

061813     move ' '                    to ws-pc-sw
061813     perform 0110-get-ermail     thru 0110-exit
061813     if (ermail-file-status = '10' or '23')
061813        and (cr-cert-sfx not = ' ')
061813        move ' '                 to cr-cert-sfx
061813        go to 0115-check-for-post-card
061813     else
061813        if ermail-file-status not = '00'
061813           go to 0115-exit
061813        end-if
061813     end-if
061813
061813     .
061813 0115-check-type.
061813
061813     perform varying m1 from +1 by +1 until
061813        (m1 > +7)
061813        or ((ma-mail-type (m1) = '1' or '2')
061813           and (ma-mail-status (m1) = '1' or '2' or '3'))
061813     end-perform
061813
061813     if m1 < +8
061813        set pc-sent              to true
061813     end-if
061813
061813     if (not pc-sent)
061813        and (cr-cert-sfx not = ' ')
061813        move ' '                 to cr-cert-sfx
061813        perform 0110-get-ermail  thru 0110-exit
061813        if ermail-file-status = '00'
061813           go to 0115-check-type
061813        end-if
061813     end-if
061813
061813     .
061813 0115-exit.
061813     exit.
      
       0125-START-ERACCT.
      
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
      
           START ERACCT KEY >= AM-CONTROL-PRIMARY
      
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
      
           .
       0125-EXIT.
           EXIT.

       0150-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              MOVE HIGH-VALUES         TO AM-CONTROL-PRIMARY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT ERROR - READ ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0150-EXIT.
           EXIT.

       0175-SYNC-CERT-ERACCT.

           IF (CR-ACCT-CONTROL > AM-CONTROL-A)
              OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                 AND (CR-DT >= AM-EXPIRE-DT))
                 PERFORM 0150-READ-ERACCT
                                       THRU 0150-EXIT
                 GO TO 0175-SYNC-CERT-ERACCT
           ELSE
              IF (CR-ACCT-CONTROL < AM-CONTROL-A)
                 OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                    AND (CR-DT < AM-EFFECT-DT))
                 DISPLAY ' NO MATCHING ACCOUNT '
                 DISPLAY ' AM CONTROL          ' AM-CONTROL-A
                 DISPLAY ' CR CONTROL          ' CR-ACCT-CONTROL
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0175-EXIT.
           EXIT.

       0200-PROCESS-ELMSTR.

           MOVE ' '                    TO WS-CLAIM-SW
                                          WS-DTH-CLAIM-SW
                                          WS-CLAIM-OPEN-SW
           move low-values             to ws-pd-thru-dt

           PERFORM 0250-START-ELMSTR   THRU 0250-EXIT
           IF ELMSTR-FILE-STATUS = '00'
              PERFORM 0260-READ-ELMSTR THRU 0260-EXIT
           ELSE
              IF ELMSTR-FILE-STATUS = '02'
                 DISPLAY ' FILE STATUS 02 ON START ' CR-CERT-NO
              END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       0250-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-BY-CERT-NO
           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD-A4
           MOVE CR-CERT-NO             TO CL-CERT-NO-A4
           MOVE CL-CONTROL-BY-CERT-NO  TO WS-SAVE-ELMSTR-KEY

           START ELMSTR KEY >= CL-CONTROL-BY-CERT-NO

           IF ELMSTR-FILE-STATUS NOT = '00' AND '10' AND '23' AND '02'
              DISPLAY ' ELMSTR ERROR - START ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0250-EXIT.
           EXIT.

       0260-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-CONTROL-BY-CERT-NO > WS-SAVE-ELMSTR-KEY)
              GO TO 0260-EXIT
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00' AND '02'
                 DISPLAY ' ELMSTR ERROR - READ ' ELMSTR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF CL-CONTROL-BY-CERT-NO = WS-SAVE-ELMSTR-KEY
              IF (CL-CERT-KEY-DATA (1:19) = CR-ACCT-CONTROL)
                 AND (WS-EFF-BIN = CL-CERT-EFF-DT)
                 SET CLAIM-FOUND       TO TRUE
                 IF CL-CLAIM-TYPE = 'L' OR 'P'
                    SET DTH-CLAIM-FOUND TO TRUE
                 ELSE
                    IF CLAIM-IS-OPEN
                       SET CLAIM-OPEN  TO TRUE
                    END-IF
                    if cl-paid-thru-dt > ws-pd-thru-dt
                       move cl-paid-thru-dt to ws-pd-thru-dt
                    end-if
                 END-IF
                 GO TO 0260-READ-ELMSTR
              ELSE
                 GO TO 0260-READ-ELMSTR
              END-IF
           END-IF

      *    IF CL-CONTROL-BY-CERT-NO = WS-SAVE-ELMSTR-KEY
      *       IF (CL-CERT-KEY-DATA (1:19) = CR-ACCT-CONTROL)
      *          AND (WS-EFF-BIN = CL-CERT-EFF-DT)
      *          AND (CL-CLAIM-TYPE = 'A')
      **         DISPLAY ' FOUND CLAIM ' CR-CERT-NO
      *          SET CLAIM-FOUND       TO TRUE
      *          IF CLAIM-IS-OPEN
      *             SET CLAIM-OPEN     TO TRUE
      *          END-IF
      *          GO TO 0260-READ-ELMSTR
      *       ELSE
      *          GO TO 0260-READ-ELMSTR
      *       END-IF
      *    END-IF

           .
       0260-EXIT.
           EXIT.

       0280-READ-CERT.

           READ CERT-FILE-IN AT END
              SET END-OF-CERT          TO TRUE
           END-READ


           IF NOT END-OF-CERT
              ADD 1                    TO CRT-RECS-IN
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           IF (EX-CITY-STATE = SPACES)
              AND (EX-ADDRESS1 = SPACES)
              IF (CR-DIS-DT NOT = ZEROS)
                 AND (CR-DISAMT > ZEROS)
                 ADD 1                 TO WS-BAD-W-CLM
              END-IF
           END-IF

031109     MOVE '*'                    TO EX-END
061813     evaluate true
061813        when ws-expire
061813           WRITE EXTR-FILE-OUT-REC1
061813                                 FROM EXTR-DETAIL-RECORD
061813           ADD 1                 TO EXT-RECS-OUT1
061813        when ws-catchup
061813           move 'Y'              to ex-catch-up
061813           display ' Found Catch up ' cr-cert-no
061813           WRITE EXTR-FILE-OUT-REC2
061813                                 FROM EXTR-DETAIL-RECORD
061813           ADD 1                 TO EXT-RECS-OUT4
061813        when other
061813           WRITE EXTR-FILE-OUT-REC2
061813                                 FROM EXTR-DETAIL-RECORD
061813           ADD 1                 TO EXT-RECS-OUT2
061813     end-evaluate

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT CERT-FILE-IN ERACCT ERMAIL ELMSTR
               OUTPUT EXTR-FILE-OUT1 EXTR-FILE-OUT2

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00'
              DISPLAY ' ELMSTR ERROR - OPEN ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE CERT-FILE-IN EXTR-FILE-OUT1 EXTR-FILE-OUT2
                 ERACCT ERMAIL ELMSTR

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELMSTR-FILE-STATUS NOT = '00'
              DISPLAY ' ELMSTR ERROR - CLOSE ' ELMSTR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
061813                                    ex-tab15
           MOVE '*'                    TO EX-END
           MOVE X'09'                  TO EX-HTAB1
                                          EX-HTAB2
                                          EX-HTAB3
                                          EX-HTAB4
                                          EX-HTAB5
                                          EX-HTAB6
                                          EX-HTAB7
                                          EX-HTAB8
                                          EX-HTAB9
                                          EX-HTAB10
                                          EX-HTAB11
                                          EX-HTAB12
                                          EX-HTAB13
                                          EX-HTAB14
061813                                    ex-htab15

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR

           MOVE EXTR-DETAIL-HEADER     TO EXTR-FILE-OUT-HEAD1
           WRITE EXTR-FILE-OUT-HEAD1

           MOVE EXTR-DETAIL-HEADER     TO EXTR-FILE-OUT-HEAD2
           WRITE EXTR-FILE-OUT-HEAD2

           PERFORM 0280-READ-CERT      THRU 0280-EXIT
           PERFORM 0125-START-ERACCT   THRU 0125-EXIT
           PERFORM 0150-READ-ERACCT    THRU 0150-EXIT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              DISPLAY ' BINARY RUN DATE ' DC-GREG-DATE-CYMD
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DT
                 DISPLAY ' THIS MONTH END DATE ' RUN-DATE
                 DISPLAY ' LAST MONTH END DATE ' WS-LAST-MONTH-DT
              ELSE
                 DISPLAY ' ERROR CREATING LAST MONTH END DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF
031109     MOVE RUN-DATE               TO WS-RUN-DT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -12                    TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-12-MONTHS-AGO
                 DISPLAY ' 12 MONTHS AGO       ' WS-12-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 12 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -13                    TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-13-MONTHS-AGO
                 DISPLAY ' 13 MONTHS AGO       ' WS-13-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 13 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

