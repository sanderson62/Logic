      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CSSNCESN.                                          
       AUTHOR.        Niall Broderick.
       DATE-WRITTEN.  July 1996.
      ******************************************************************
      *+--------------------------------------------------------------+*
      *|To compile this program type 'make CSSNCESN' in $UNIKIX/src   |*
      *|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|*
      *|This program is invoked by transactions CSSN & CESN           |*
      *|                                                              |*
      *|format:                                                       |*
      *|                                                              |*
      *|CSSN or CESN with no parameters will put a map on the screen  |*
      *|                                                              |*
      *|otherwise:                                                    |*
      *|                                                              |*
      *|CSSN NAME=operator name, PS=password                          |*
      *|                                                              |*
      *|or                                                            |*
      *|                                                              |*
      *|CESN USERID=userid, PS=password[, NEWPS=new password]         |*
      *+--------------------------------------------------------------+*
      ******************************************************************
      
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ***************
      * "CONSTANTS" *
      ***************

       77  C-TN3270                        PIC X     VALUE 'T'.
       77  C-DATE-SEP                      PIC X     VALUE '/'.
       77  C-TIME-SEP                      PIC X     VALUE ':'.
       77  C-COMMAS                        PIC X     VALUE ','.
       77  C-NO-OF-TRIES-ALLOWED           PIC S9(4) COMP VALUE 3.
       77  C-TIME-INTERVAL                 PIC S9(7) COMP-3 VALUE 4.
       77  C-FN-SIGNON                     PIC X(2)  VALUE X"7402".
       77  C-FN-SIGNOFF                    PIC X(2)  VALUE X"7404".
       77  C-TRUE                          PIC X     VALUE 'Y'.
       77  C-FALSE                         PIC X     VALUE 'N'.
       77  C-TCT-ENTRY-NOT-FOUND           PIC S9(8) COMP VALUE -1.
       77  C-TCT-INDEX                     PIC S9(8) COMP VALUE ZERO.
       77  C-ASKIP-DRK                     PIC X     VALUE '@'.
       77  C-CESN-MAP-NAME                 PIC X(8)  VALUE 'CCESNMP'.
       77  C-CSSN-MAP-NAME                 PIC X(8)  VALUE 'CCSSNMP'.
       77  C-MAP-SET-NAME                  PIC X(8)  VALUE 'CSIGNMP'.
       77  C-CESN-TRAN-ID                  PIC X(4)  VALUE 'CESN'.
       77  C-CSSN-TRAN-ID                  PIC X(4)  VALUE 'CSSN'.
       77  C-SIGN-ON-OPERATOR              PIC X(26)
                                 VALUE '94N                   #  N'.
       77  C-CICS-ISSUE-DISCONNECT         PIC X(23)
                                 VALUE '97Y                !   '.
      *external C routines within unikixtran
       77  C-TRANS                  PIC X(8)  VALUE 'kxdfhei1'.
       77  C-GET-TERM-TYPE          PIC X(9)  VALUE 'kxtct3270'.
       77  C-DOES-TERMID-EXIST      PIC X(12) VALUE 'kxcobfindtct'.
       77  C-CHANGE-TERM-ID         PIC X(15) VALUE 'kx3270assgnterm'.
      *not used as default. contained in commented out code.
       77  C-APPLICATION-TRANSID           PIC X(4)  VALUE 'XXXX'.
       77  C-DUMMY-OP-NAME                 PIC X(20) VALUE SPACES.
       77  C-DUMMY-PASSWORD                PIC X(8)  VALUE SPACES.
      * EIBRESP2 values
       77  C-TERM-ID-OK                    PIC S9(4) COMP VALUE ZERO.
       77  C-SIGNON-SUCCESSFUL             PIC S9(4) COMP VALUE ZERO.
       77  C-INVALID-PASSWORD              PIC S9(4) COMP VALUE 2.
       77  C-PASSWORD-EXPIRED              PIC S9(4) COMP VALUE 3.
       77  C-INVALID-USERID                PIC S9(4) COMP VALUE 8.
       77  C-TERMID-ALREADY-SIGNED-ON      PIC S9(4) COMP VALUE 9.
       77  C-PASSWORD-SUSPENDED            PIC S9(4) COMP VALUE 19.
      * made new eibresp2 codes
       77  C-TERMID-EXISTS                 PIC S9(4) COMP VALUE 501.
       77  C-NEW-PASSWORD-NOT-VERIFIED     PIC S9(4) COMP VALUE 502.
       77  C-SIGNON-CANCELLED              PIC S9(4) COMP VALUE 503.
       77  C-INVALID-PARAMETER             PIC S9(4) COMP VALUE 504.
       77  C-TRAN-ABENDED                  PIC S9(4) COMP VALUE 505.
       77  C-UNKNOWN-ERROR                 PIC S9(4) COMP VALUE 506.
       77  C-OPERATOR-SUCCESSFUL           PIC S9(4) COMP VALUE 507.
       77  C-OPERATOR-NAME-INVALID         PIC S9(4) COMP VALUE 508.
       77  C-OPERATOR-PASSWORD-INVALID     PIC S9(4) COMP VALUE 509.

      *********************
      * boolean variables *
      *********************

       01  PARAMETERS-PASSED               PIC X     VALUE 'Y'.
           88 B-PARAMETERS-PASSED                    VALUE 'Y'.
       01  INVALID-PARAMETER               PIC X     VALUE 'Y'.
           88 B-INVALID-PARAMETER                    VALUE 'Y'.

      **********************
      * standard variables *
      **********************
       01  ABSTIME                         PIC S9(15) COMP-3 VALUE ZERO.
       01  LINE-BUFFER.
           05 TRAN-ID                      PIC X(4)  VALUE SPACES.
           05 SEPERATOR                    PIC X     VALUE SPACE.
           05 LINE-BUFFER-PARMS            PIC X(74) VALUE SPACES.
       01  PARM                  PIC X(25) OCCURS 4 TIMES VALUE SPACES.
       01  PARM-KEYWORD                    PIC X(6)  VALUE SPACES.
           88  USERID                VALUE 'USERID' 
                                           'userid'.
           88  OPNAME                VALUE 'NAME' 
                                           'name'.
           88  PS                    VALUE 'PS' 
                                           'ps'.
           88  NEWPS                 VALUE 'NEWPS' 
                                           'newps'.
       01  KIX-DATE              PIC X(8)  VALUE SPACES.
       01  KIX-TIME              PIC X(8)  VALUE SPACES.
      * Case #1656 change size to 20 to fit operator name.
       01  PARM-VARIABLE         PIC X(20) VALUE SPACES.
       01  NO-OF-PARMS           PIC S9(4) COMP VALUE ZERO.
       01  LOOP-CTR              PIC S9(4) COMP VALUE ZERO.
       01  EIB-FUNCTION          PIC X(2)       VALUE SPACES.
       01  EIB-FUNCTION-COMP     PIC 9(4)  COMP REDEFINES EIB-FUNCTION.

       01  ERROR-CODE             VALUE LOW-VALUES.
           05 FILLER              PIC X.
           05 ECODE               PIC X.
       01  ERR-CODE-RDF           REDEFINES ERROR-CODE.
           05  ERR-CODE               PIC 9(4) COMP.
               88  VALIDC             VALUE 0.
               88  INVCSSN            VALUE 1.
               88  INVOPNM            VALUE 2.
               88  INVPSWD            VALUE 3.
               88  PASWEXP            VALUE 4.
               88  INVSUSP            VALUE 5.
               88  MAPERR             VALUE 6.

      ******************
      * error messages *
      ******************

       01  MSG                 PIC X(79) VALUE SPACES.

       01  KIX0458E.
           05  FILLER          PIC X(9)  VALUE 'KIX0458E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(8)  VALUE ' Userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(15) VALUE ' not recognized'.

       01  KIX0460E.
           05  FILLER          PIC X(9)  VALUE 'KIX0460E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(29)
                          VALUE ' Password invalid for userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.

       01  KIX0461E.
           05  FILLER          PIC X(9)  VALUE 'KIX0461E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(41)
                  VALUE ' New password is not accepted for userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.

       01  KIX0463I.
           05  FILLER          PIC X(9)  VALUE 'KIX0463I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(8)  VALUE ' Userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(20) VALUE ' signed on terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.

       01  KIX0462I.
           05  FILLER          PIC X(9)  VALUE 'KIX0462I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(21) VALUE ' Sign on transaction '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' cancelled'.

       01  KIX0490E.
           05  FILLER          PIC X(9)  VALUE 'KIX0490E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(15) VALUE ' Unknown error '.
           05  EIB-RESP-2      PIC 9(9)  VALUE ZERO.
           05  FILLER          PIC X(16) VALUE ' in transaction '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.

       01  KIX0491E.
           05  FILLER          PIC X(9)  VALUE 'KIX0491E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Operator '.
           05  OP-NAME         PIC X(20) VALUE SPACES.
           05  FILLER          PIC X(15) VALUE ' not recognized'.

       01  KIX0492E.
           05  FILLER          PIC X(9)  VALUE 'KIX0492E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(31)
                          VALUE ' Password invalid for operator '.
           05  OP-NAME         PIC X(20)  VALUE SPACES.

       01  KIX0493I.
           05  FILLER          PIC X(9)  VALUE 'KIX0493I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Operator '.
           05  OP-NAME         PIC X(20) VALUE SPACES.
           05  FILLER          PIC X(20) VALUE ' signed on terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.

       01  KIX0494S.
           05  FILLER          PIC X(9)  VALUE 'KIX0494S '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(6)  VALUE ' Tran '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(18) VALUE ' abended: EIBFN = '.
           05  EIB-FN          PIC 9(5)  VALUE ZERO.

       01  KIX0495E.
           05  FILLER          PIC X(9)  VALUE 'KIX0495E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(7)  VALUE ' Usage '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.
           05  USAGE-TEXT      PIC X(50) VALUE SPACES.

       01  KIX0496E.
           05  FILLER          PIC X(9)  VALUE 'KIX0496E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(29)
                  VALUE ' Password expired for Userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(17)
                  VALUE ' - must change PW'.

       01  KIX0498E.
           05  FILLER          PIC X(9)  VALUE 'KIX0498E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(30)
                  VALUE ' Password disabled for userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.

       77  KIX0495-USER-TEXT   PIC X(50) VALUE
               ' [USERID=userid[,PS=password[,NEWPS=newpassword]]]'.
       77  KIX0495-NAME-TEXT   PIC X(35) VALUE
               ' [NAME=operator name[,PS=password]]'.

       01  KIX0496I.
           05  FILLER          PIC X(9)  VALUE 'KIX0496I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(31)
                         VALUE ' is already logged on elsewhere'.

       01  KIX0497E.
           05  FILLER          PIC X(9)  VALUE 'KIX0497E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(40)
                     VALUE ' is already signed on, you must sign off'.

      *************
      * copybooks *
      *************
       COPY CSIGNMP.

       COPY DFHAID.

      ***********************
      * communications area *
      ***********************
       01  COMM-AREA.
           05  COMM-APPL-ID           PIC X(8)        VALUE SPACES.
           05  COMM-NO-OF-TRIES       PIC S9(4) COMP  VALUE ZERO.
           05  COMM-TERM-INDICATOR    PIC X           VALUE SPACE.


      ***************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA                   PIC X(11).

      ********
      * main *
      ********
       PROCEDURE DIVISION.

       MAIN SECTION.

      ******************************************
      * do transaction specific initialization *
      ******************************************
           PERFORM INITIALIZATION.

      ********************************************
      * if start of logical transaction          *
      *    do first time specific initialization *
      *    if cesn/cssn was passed parameters    *
      *       exit                               * 
      *    else                                  *
      *       exit with return tranid            *
      *    end if                                *
      * else                                     *
      *    restore communications area           *
      *    receive data from screen.             *
      ********************************************
           IF EIBCALEN = ZERO
              PERFORM FIRST-TIME-IN
              IF B-PARAMETERS-PASSED
                 PERFORM DISPLAY-TEXT-MESSAGE
                 GO TO EXIT-RETURN
              ELSE
                 GO TO EXIT-RETURN-TRAN-ID 
              END-IF
           ELSE
              MOVE DFHCOMMAREA TO COMM-AREA
              PERFORM ACCEPT-SCREEN-INPUT
           END-IF.

      **************************************************
      * if clear key was pressed exit from transaction *
      * else process screen information.               *
      **************************************************
           IF EIBAID = DFHCLEAR
              MOVE C-SIGNON-CANCELLED TO EIBRESP2
              PERFORM DISPLAY-TEXT-MESSAGE
           ELSE

      ***********************************
      * validate input fields on screen *
      * and try to sign on.             *
      * return value: EIBRESP2          *
      ***********************************
              PERFORM VALIDATE-TERMINAL-ID
              IF EIBRESP2 = C-TERM-ID-OK
                 PERFORM VALIDATE-SIGNON
              END-IF

      ************************************************************
      * if signon was successful                                 *
      *    display message                                       *
      *    update terminal id                                    *
      *    exit normally                                         *
      * else                                                     *
      *    if not greater than no. of unsuccessful tries allowed *
      *       display relevant error message                     *
      *       exit with return tranid                            *
      *    else too many unsuccessful tries                      *
      *       display error message on screen                    *
      *       erase screen                                       *
      *       display cancelled error on screen                  *
      *       exit from transaction.                             *
      ************************************************************
              IF EIBRESP2 = C-SIGNON-SUCCESSFUL OR
                            C-OPERATOR-SUCCESSFUL
                 PERFORM UPDATE-TERMINAL-ID
                 PERFORM DISPLAY-TEXT-MESSAGE
                 GO TO SUCCESSFUL-EXIT-RETURN
              ELSE
                 IF COMM-NO-OF-TRIES < C-NO-OF-TRIES-ALLOWED
                    PERFORM DISPLAY-SCREEN-MESSAGE
                    GO TO EXIT-RETURN-TRAN-ID
                 ELSE
                    PERFORM DISPLAY-SCREEN-MESSAGE
                    EXEC CICS DELAY INTERVAL(C-TIME-INTERVAL)
                    END-EXEC
                    MOVE C-SIGNON-CANCELLED TO EIBRESP2
                    PERFORM DISPLAY-TEXT-MESSAGE
                 END-IF
              END-IF
           END-IF.


      *********************************************************
      * uncomment the following code if you want the terminal *
      * to logoff when you press CLEAR or after n no. of      *
      * unsuccessful tries.                                   *
      * n being the variable C-NO-OF-TRIES-ALLOWED            *
      *********************************************************
       EXIT-LOGOFF-TERMINAL.

      *    PERFORM LOGOFF-TERMINAL.

      ****************************************************
      * return to native cics if the above code does not *
      * logoff the terminal.                             *
      ****************************************************
           EXEC CICS SEND CONTROL FREEKB END-EXEC.
           EXEC CICS RETURN END-EXEC.


      ********************************************************
      * return with tranid to accept another sign on request *
      ********************************************************
       EXIT-RETURN-TRAN-ID.

           EXEC CICS RETURN TRANSID(EIBTRNID)
                            COMMAREA(COMM-AREA)
                            LENGTH(LENGTH OF COMM-AREA)
           END-EXEC.


      ***********************************************************
      * uncomment the following CICS START code if you wish to  *
      * start a transaction directly after a successful signon. *
      * change the contents of C-APPLICATION-TRANSID to the     *
      * transaction you want started.                           *
      ***********************************************************
       SUCCESSFUL-EXIT-RETURN.

      *    EXEC CICS START TRANSID(C-APPLICATION-TRANSID)
      *                    TERMID(EIBTRMID)
      *    END-EXEC.
      *    EXEC CICS DELAY INTERVAL(C-TIME-INTERVAL) END-EXEC.

       EXIT-RETURN.

           EXEC CICS SEND CONTROL FREEKB END-EXEC.
           EXEC CICS RETURN END-EXEC.

       MAIN-EXIT.

           EXIT.


      *   +----------------------------------------+
      ***/| logoff current terminal                |
      * / |                                        |
      * | |  input(s): none                        |
      * | | output(s): none                        |
      * | +----------------------------------------+
      * |/                                        /
      * /________________________________________/*
      ********************************************* 
       LOGOFF-TERMINAL SECTION.

           EXEC CICS DELAY INTERVAL(C-TIME-INTERVAL) END-EXEC.
           MOVE SPACES TO C-DUMMY-PASSWORD C-DUMMY-OP-NAME.
           MOVE C-CICS-ISSUE-DISCONNECT TO DFHEIV0.
           CALL C-TRANS USING DFHEIV0, C-DUMMY-PASSWORD,
                                        C-DUMMY-OP-NAME.

       LOGOFF-TERMINAL-EXIT.

           EXIT.


      *   +----------------------------------------+
      ***/| do transaction specific initialization |
      * / |                                        |
      * | |  input(s): none                        |
      * | | output(s): none                        |
      * | +----------------------------------------+
      * |/                                        /
      * /________________________________________/*
      ********************************************* 
       INITIALIZATION SECTION.

           EXEC CICS IGNORE CONDITION INVREQ
                                      NOTAUTH
                                      MAPFAIL
                                      LENGERR
           END-EXEC.


       INITIALIZATION-EXIT.

           EXIT.


      *   +----------------------------------------+
      ***/| get date and time                      |
      * / |                                        |
      * | |  input(s): none                        |
      * | | output(s): ESNDATEO                    |
      * | |          : ESNTIMEO                    |
      * | |          : SSNDATEO                    |
      * | |          : SSNTIMEO                    |
      * | +----------------------------------------+
      * |/                                        / 
      * /________________________________________/* 
      ********************************************* 
       GET-DATE-TIME SECTION.

           EXEC CICS
               ASKTIME ABSTIME(ABSTIME)
           END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(ABSTIME)
                                DATESEP(C-DATE-SEP) MMDDYY(KIX-DATE)
                                TIMESEP(C-TIME-SEP) TIME(KIX-TIME)
           END-EXEC.

           IF EIBTRNID = C-CESN-TRAN-ID
              MOVE KIX-DATE TO ESNDATEO
              MOVE KIX-TIME TO ESNTIMEO
           ELSE
              MOVE KIX-DATE TO SSNDATEO
              MOVE KIX-TIME TO SSNTIMEO
           END-IF.

        GET-DATE-TIME-EXIT.

           EXIT.


      *   +----------------------------------------+
      ***/| process eibresp2 code and pass back a  |
      * / | corresponding error message and update |
      * | | the no. of tries counter.              |
      * | |                                        |
      * | |  input(s): EIBRESP2                    |
      * | |          : COMM-APPL-ID                |
      * | |          : USERIDI                     |
      * | |          : EIB-FUNCTION-COMP           |
      * | | output(s): MSG                         |
      * | +----------------------------------------+
      * |/                                        / 
      * /________________________________________/* 
      ********************************************* 
       PROCESS-ERROR-CODE SECTION.

           EVALUATE EIBRESP2

               WHEN C-SIGNON-SUCCESSFUL
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0463I
                       MOVE USERIDI TO USER-ID OF KIX0463I
                       MOVE EIBTRMID TO TERM-ID OF KIX0463I
                       MOVE KIX0463I TO MSG

               WHEN C-INVALID-PASSWORD
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0460E
                       MOVE USERIDI TO USER-ID OF KIX0460E
                       MOVE KIX0460E TO MSG

               WHEN C-INVALID-USERID
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0458E
                       MOVE USERIDI TO USER-ID OF KIX0458E
                       MOVE KIX0458E TO MSG

               WHEN C-TERMID-EXISTS
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0496I
                       MOVE TERMIDI TO TERM-ID OF KIX0496I
                       MOVE KIX0496I TO MSG

               WHEN C-TERMID-ALREADY-SIGNED-ON
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0497E
                       MOVE TERMIDI TO TERM-ID OF KIX0497E
                       MOVE KIX0497E TO MSG

               WHEN C-PASSWORD-EXPIRED
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0496E
                       MOVE USERIDI TO USER-ID OF KIX0496E
                       MOVE KIX0496E TO MSG

               WHEN C-PASSWORD-SUSPENDED
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0498E
                       MOVE USERIDI TO USER-ID OF KIX0498E
                       MOVE KIX0498E TO MSG

               WHEN C-NEW-PASSWORD-NOT-VERIFIED
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0461E
                       MOVE USERIDI TO USER-ID OF KIX0461E
                       MOVE KIX0461E TO MSG

               WHEN C-SIGNON-CANCELLED
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0462I
                       MOVE EIBTRNID TO TRAN-ID OF KIX0462I
                       MOVE KIX0462I TO MSG

               WHEN C-INVALID-PARAMETER
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0495E
                       MOVE EIBTRNID TO TRAN-ID OF KIX0495E
                       IF EIBTRNID = C-CESN-TRAN-ID
                          MOVE KIX0495-USER-TEXT TO USAGE-TEXT
                       ELSE
                          MOVE KIX0495-NAME-TEXT TO USAGE-TEXT
                       END-IF
                       MOVE KIX0495E TO MSG

               WHEN C-TRAN-ABENDED
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0494S
                       MOVE EIBTRNID TO TRAN-ID OF KIX0494S
                       MOVE EIB-FUNCTION-COMP TO EIB-FN OF KIX0494S
                       MOVE KIX0494S TO MSG

               WHEN C-OPERATOR-NAME-INVALID
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0491E
                       MOVE OPNAMEI TO OP-NAME OF KIX0491E
                       MOVE KIX0491E TO MSG

               WHEN C-OPERATOR-PASSWORD-INVALID
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0492E
                       MOVE OPNAMEI TO OP-NAME OF KIX0492E
                       MOVE KIX0492E TO MSG

               WHEN C-OPERATOR-SUCCESSFUL
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0493I
                       MOVE OPNAMEI TO OP-NAME OF KIX0493I
                       MOVE EIBTRMID TO TERM-ID OF KIX0493I
                       MOVE KIX0493I TO MSG

               WHEN OTHER
                       MOVE COMM-APPL-ID TO APPL-ID OF KIX0490E
                       MOVE EIBRESP2 TO EIB-RESP-2 OF KIX0490E
                       MOVE EIBTRNID TO TRAN-ID OF KIX0490E
                       MOVE KIX0490E TO MSG

           END-EVALUATE.

       PROCESS-ERROR-CODE-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| pass EIBRESP2 to PROCESS-ERROR-CODE and display the |
      * / | returned message to the map screen.                 |
      * | |                                                     |
      * | |  input(s): EIBRESP2                                 |
      * | | output(s): MSG displayed on map screen              |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-SCREEN-MESSAGE SECTION.

           PERFORM PROCESS-ERROR-CODE.
           IF EIBTRNID = C-CESN-TRAN-ID
              MOVE MSG TO ESNMSGO
           ELSE
              MOVE MSG TO SSNMSGO
           END-IF
           PERFORM DISPLAY-SCREEN.

       DISPLAY-SCREEN-MESSAGE-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| pass EIBRESP2 to PROCESS-ERROR-CODE and display the |
      * / | returned message on the top line of a cleared       |
      * | | screen (no map).                                    |
      * | |                                                     |
      * | |  input(s): EIBRESP2                                 |
      * | | output(s): MSG displayed on top line of screen      |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-TEXT-MESSAGE SECTION.

           PERFORM PROCESS-ERROR-CODE.
           EXEC CICS SEND TEXT
                     FROM(MSG)
                     LENGTH(LENGTH OF MSG)
                     ERASE
                     FREEKB
           END-EXEC.

       DISPLAY-TEXT-MESSAGE-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| do first time specific initialization once per      |
      * / | logical transaction.                                |
      * | |                                                     |
      * | |  input(s): none                                     |
      * | | output(s): COMM-NO-OF-TRIES                         |
      * | |          : COMM-TERM-INDICATOR                      |
      * | |          : COMM-APPL-ID                             |
      * | |          : EIBRESP2                                 |
      * | |          : PARAMETERS-PASSED (boolean)              |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       FIRST-TIME-IN SECTION.

      * find out what the application id is (used in error messages)
           EXEC CICS ASSIGN APPLID(COMM-APPL-ID) END-EXEC.
      * sign off any current user that is logged on
           PERFORM SIGN-OFF-USER.

      * receive command line into line-buffer
           EXEC CICS RECEIVE
                     INTO(LINE-BUFFER)
                     LENGTH(LENGTH OF LINE-BUFFER)
                     ASIS
           END-EXEC.

      **********************************
      * if parameters were passed      *
      *    process the parameters      *
      * else                           *
      *    transaction will send a map *
      **********************************
           IF LINE-BUFFER-PARMS NOT = SPACES
              MOVE C-TRUE TO PARAMETERS-PASSED
              PERFORM PROCESS-PARAMETERS
           ELSE
              MOVE C-FALSE TO PARAMETERS-PASSED
              PERFORM FIRST-TIME-IN-MAP.

       FIRST-TIME-IN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| try and sign on the user and then update the term id|
      * | |                                                     |
      * | |  input(s): USERIDI                                  |
      * | |          : PASSWRDI                                 |
      * | |          : NEWPASSI                                 |
      * | |          : OPNAMEI                                  |
      * | |          : SSNPASSI                                 |
      * | |          : COMM-NO-OF-TRIES                         |
      * | | output(s): EIBRESP2                                 |
      * | |          : COMM-NO-OF-TRIES                         |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-ON-USER SECTION.

           ADD 1 TO COMM-NO-OF-TRIES.
           IF EIBTRNID = C-CESN-TRAN-ID
              PERFORM SIGN-ON-USER-CESN
           ELSE
              PERFORM SIGN-ON-USER-CSSN
           END-IF.

       SIGN-ON-USER-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| try and sign on the user using cssn                 |
      * | |                                                     |
      * | |  input(s): OPNAMEI                                  |
      * | |          : SSNPASSI                                 |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-ON-USER-CSSN SECTION.

      * call to UniKix to sign on with op-name and password
           MOVE C-SIGN-ON-OPERATOR TO DFHEIV0.
           CALL C-TRANS USING DFHEIV0, SSNPASSI, OPNAMEI.

      * evaluate return code and set up eibresp2 with relevant value
           MOVE EIBRCODE TO ECODE.
           IF VALIDC
              MOVE C-OPERATOR-SUCCESSFUL TO EIBRESP2
           ELSE IF INVOPNM
              MOVE C-OPERATOR-NAME-INVALID TO EIBRESP2
           ELSE IF INVPSWD
              MOVE C-OPERATOR-PASSWORD-INVALID TO EIBRESP2
           ELSE IF PASWEXP
              MOVE C-PASSWORD-EXPIRED TO EIBRESP2
           ELSE IF INVSUSP
              MOVE C-PASSWORD-SUSPENDED TO EIBRESP2
	   ELSE
              MOVE C-UNKNOWN-ERROR TO EIBRESP2
           END-IF.

       SIGN-ON-USER-CSSN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| try and sign on the user using cesn                 |
      * | |                                                     |
      * | |  input(s): USERIDI                                  |
      * | |          : PASSWRDI                                 |
      * | |          : NEWPASSI                                 |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-ON-USER-CESN SECTION.

      *************************************************************        
      * at this time IGNORE condition is not supported for SIGNON *
      * but the handle abend will catch any condition and return  *
      * to the paragraph SIGN-ON-USER-EXIT via the abend routine  *
      * called ABEND-ROUTINE.                                     *
      *************************************************************
           EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) END-EXEC.
           IF NEWPASSI NOT = SPACES
              EXEC CICS SIGNON
                        USERID(USERIDI)
                        PASSWORD(PASSWRDI)
                        NEWPASSWORD(NEWPASSI)
              END-EXEC
           ELSE
              EXEC CICS SIGNON
                        USERID(USERIDI)
                        PASSWORD(PASSWRDI)
              END-EXEC 
           END-IF.

       SIGN-ON-USER-CESN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| process possible parameters USERID/NAME, PS, (NEWPS)|
      * / | and try to sign on with these parameters            |
      * | |                                                     |
      * | |  input(s): LINE-BUFFER                              |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       PROCESS-PARAMETERS SECTION.

      * make all delimiters spaces
           INSPECT LINE-BUFFER-PARMS REPLACING ALL C-COMMAS BY SPACES.
      * separate all possible parameters
           UNSTRING LINE-BUFFER-PARMS
                    DELIMITED BY ALL SPACES
                    INTO PARM(1)
                         PARM(2)
                         PARM(3)
                         PARM(4)
           END-UNSTRING.

           MOVE SPACES TO USERIDI PASSWRDI NEWPASSI.
           MOVE C-FALSE TO INVALID-PARAMETER.
           MOVE ZERO TO NO-OF-PARMS.
      * process all 4 parameters
           PERFORM VARYING LOOP-CTR FROM 1 BY 1 UNTIL LOOP-CTR > 4

      * separate keyword and variable of each parameter
               UNSTRING PARM(LOOP-CTR)
                        DELIMITED BY '='
                        INTO PARM-KEYWORD
                             PARM-VARIABLE
               END-UNSTRING
               IF USERID AND EIBTRNID = C-CESN-TRAN-ID
                  ADD 1 TO NO-OF-PARMS
                  MOVE PARM-VARIABLE TO USERIDI
               ELSE IF OPNAME AND EIBTRNID = C-CSSN-TRAN-ID
                  ADD 1 TO NO-OF-PARMS
                  MOVE PARM-VARIABLE TO OPNAMEI
               ELSE IF PS
                  ADD 1 TO NO-OF-PARMS
                  IF EIBTRNID = C-CESN-TRAN-ID
                     MOVE PARM-VARIABLE TO PASSWRDI
                  ELSE
                     MOVE PARM-VARIABLE TO SSNPASSI
                  END-IF
               ELSE IF NEWPS AND EIBTRNID = C-CESN-TRAN-ID
                  ADD 1 TO NO-OF-PARMS
                  MOVE PARM-VARIABLE TO NEWPASSI
               ELSE IF PARM-KEYWORD NOT = SPACES
                  MOVE C-TRUE TO INVALID-PARAMETER
               END-IF 
           END-PERFORM.

      *******************************************************
      * if no parameters were found or an invalid parameter *
      *    reject sign on attempt                           *
      * else                                                *
      *    sign on                                          *
      *******************************************************
           IF NO-OF-PARMS = ZERO OR B-INVALID-PARAMETER
              MOVE C-INVALID-PARAMETER TO EIBRESP2
           ELSE
              PERFORM SIGN-ON-USER
           END-IF.

       PROCESS-PARAMETERS-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| do first time specific initialization once per      |
      * / | logical transaction.                                |
      * | |                                                     |
      * | |  input(s): none                                     |
      * | | output(s): COMM-NO-OF-TRIES                         |
      * | |          : COMM-TERM-INDICATOR                      |
      * | |          : COMM-APPL-ID                             |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       FIRST-TIME-IN-MAP SECTION.

      ***************************************************
      * find out what type the current terminal is      *
      *     T = tn3270                                  *
      * other = we are not interested in any other type *
      ***************************************************
           CALL C-GET-TERM-TYPE USING COMM-TERM-INDICATOR.

      * about to attempt first try
           MOVE 1 TO COMM-NO-OF-TRIES.
      * display initial map screen
           PERFORM DISPLAY-SCREEN.

       FIRST-TIME-IN-MAP-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| display map screen.                                 |
      * | |                                                     |
      * | |  input(s): COMM-TERM-INDICATOR                      |
      * | |          : CESNMAPO                                 |
      * | | output(s): displayed map screen                     |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-SCREEN SECTION.

      * populate map with current date and time.
           PERFORM GET-DATE-TIME.

      ************************************************************
      * if current tran is 'cesn' and terminal in not a tn3270   *
      *    make the terminal related fields on the map invisible *
      ************************************************************
           IF EIBTRNID = C-CESN-TRAN-ID AND
              COMM-TERM-INDICATOR NOT = C-TN3270
                MOVE C-ASKIP-DRK TO TERM-IDA TERMIDA
           END-IF.

           IF EIBTRNID = C-CESN-TRAN-ID
              EXEC CICS
                   SEND MAP (C-CESN-MAP-NAME)
                        MAPSET(C-MAP-SET-NAME)
                        FROM(CCESNMPO)
                        LENGTH(LENGTH OF CCESNMPO)
                        ERASE
                        FREEKB
              END-EXEC
           ELSE
      * must be tranid 'cssn'
              EXEC CICS
                   SEND MAP (C-CSSN-MAP-NAME)
                        MAPSET(C-MAP-SET-NAME)
                        FROM(CCSSNMPO)
                        LENGTH(LENGTH OF CCSSNMPO)
                        ERASE
                        FREEKB
              END-EXEC
           END-IF.

       DISPLAY-SCREEN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| receive input from map screen.                      |
      * | |                                                     |
      * | |  input(s): map screen                               |
      * | | output(s): CESNMAPI                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       ACCEPT-SCREEN-INPUT SECTION.

           IF EIBTRNID = C-CESN-TRAN-ID
              EXEC CICS
                   RECEIVE MAP (C-CESN-MAP-NAME)
                           MAPSET(C-MAP-SET-NAME)
                           INTO(CCESNMPI)
                           ASIS
              END-EXEC
           ELSE
      * must be tranid 'cssn'
              EXEC CICS
                   RECEIVE MAP (C-CSSN-MAP-NAME)
                           MAPSET(C-MAP-SET-NAME)
                           INTO(CCSSNMPI)
                           ASIS
              END-EXEC
           END-IF.

       ACCEPT-SCREEN-INPUT-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| validate screen TERM-ID to see if it can be used.   |
      * | |                                                     |
      * | |  input(s): COMM-TERM-INDICATOR                      |
      * | |          : TERMIDI                                  |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       VALIDATE-TERMINAL-ID SECTION.

      *****************************************************
      * if current tran is CESN and                       *
      *            current terminal is a tn3270 and       *
      *            TERMID field on map screen has a value *
      *    check tct for TERMID already existing          *
      *    if TERMID does not exist in tct                *
      *       accept TERMID                               *
      *    else                                           *
      *       reject TERMID.                              *
      *****************************************************
           IF EIBTRNID = C-CESN-TRAN-ID AND
              COMM-TERM-INDICATOR = C-TN3270 AND
              TERMIDI NOT = SPACES
                CALL C-DOES-TERMID-EXIST USING TERMIDI, C-TCT-INDEX
                IF C-TCT-INDEX = C-TCT-ENTRY-NOT-FOUND
                   MOVE C-TERM-ID-OK TO EIBRESP2
                ELSE
                   MOVE C-TERMID-EXISTS TO EIBRESP2
                END-IF
           END-IF.

       VALIDATE-TERMINAL-ID-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| overwrite current TERMID to TERMID on screen.       |
      * | |                                                     |
      * | |  input(s): COMM-TERM-INDICATOR                      |
      * | |          : TERMIDI                                  |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       UPDATE-TERMINAL-ID SECTION.

      ****************************************************************
      * if current tran is CESN and current terminal is a tn3270 and *
      *            TERMID field on map screen has a value            *
      *    overwrite current TERMID in tct with TERMID from screen.  *
      ****************************************************************
           IF EIBTRNID = C-CESN-TRAN-ID AND
              COMM-TERM-INDICATOR = C-TN3270 AND
              TERMIDI NOT = SPACES
                CALL C-CHANGE-TERM-ID USING TERMIDI
           END-IF.

       UPDATE-TERMINAL-ID-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| sign off user                                       |
      * | |                                                     |
      * | |  input(s): none                                     |
      * | | output(s): none                                     |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-OFF-USER SECTION.

      **************************************************************
      * at this time IGNORE condition is not supported for SIGNOFF *
      * but the handle abend will catch any condition and return   *
      * to the paragraph SIGN-OFF-USER-EXIT via the abend routine  *
      * called ABEND-ROUTINE.                                      *
      **************************************************************
           EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) END-EXEC.
           EXEC CICS SIGNOFF END-EXEC.
           CONTINUE.

       SIGN-OFF-USER-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| validate sign on screen input and sign on user if   |
      * / | screen input is ok.                                 |
      * | |                                                     |
      * | |  input(s): COMM-TERM-INDICATOR                      |
      * | |          : TERMIDI                                  |
      * | |          : USERIDI                                  |
      * | |          : PASSWRDI                                 |
      * | |          : NEWPASSI                                 |
      * | |          : NWPASS2I                                 |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       VALIDATE-SIGNON SECTION.

      *****************************************************************
      * if user wants to change password but password is not verified *
      *    reject new password and don't try to sign on               *
      * else                                                          *
      *    try to sign on                                             *
      * endif                                                         *
      *****************************************************************
           IF EIBTRNID = C-CESN-TRAN-ID AND
              NEWPASSI NOT = SPACES AND
              NEWPASSI NOT = NWPASS2I
               MOVE C-NEW-PASSWORD-NOT-VERIFIED TO EIBRESP2
           ELSE
               PERFORM SIGN-ON-USER
           END-IF.

       VALIDATE-SIGNON-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| catch all abends.                                   |
      * | |                                                     |
      * | |  input(s): EIBFN                                    |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       ABEND-ROUTINE SECTION.

      ***************************************************************        
      * at this time IGNORE condition is not supported for SIGNON   *
      * or SIGNOFF.  The abend is captured in this routine and if   *
      * it was caused by a SIGNON or SIGNOFF a GO TO is executed to *
      * mimic an IGNORE condition.                                  *
      ***************************************************************

           EVALUATE EIBFN

              WHEN C-FN-SIGNON
                   GO TO SIGN-ON-USER-CESN-EXIT

              WHEN C-FN-SIGNOFF
                   GO TO SIGN-OFF-USER-EXIT

              WHEN OTHER
                   EXEC CICS HANDLE ABEND END-EXEC
                   MOVE EIBFN TO EIB-FUNCTION
                   MOVE C-TRAN-ABENDED TO EIBRESP2
                   PERFORM DISPLAY-TEXT-MESSAGE
                   EXEC CICS ABEND END-EXEC

           END-EVALUATE.
