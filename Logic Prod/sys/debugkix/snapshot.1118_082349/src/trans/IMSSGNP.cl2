      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    IMSSGNP.                                          
       AUTHOR.        Dell.
       DATE-WRITTEN.  June 2010.
      *
      * Custom.
      *
      * The following are the configuration requirements to use this 
      * program and its associated maps DFSDF1 and DFSDF2
      *
      * In the PCT table:
      *
      *           Task  Trans TWA  Trans Scrn     File        Remote
      * Program  Requst  ID   Size  Sec  Size Acct ID  SysID  TranCd
      * IMSSGNP         /SIG          1  DEF   D                    
      * IMSSGNP         TSIG          1  DEF   D                    
      *
      *
      * In the PPT table:
      *
      * Program   Typ  SysID  RmtProg   RmtTrn  Group      Shared Library
      * IMSSGNP   C
      * DFSDF1    M
      * DFSDF2    M
      *
      *
      * In the PLT table:
      *
      * Program           Program
      *  Name              Type               Group
      * IMSSGNP           USERSTART
      *
      *
      * For the TN3270 configuration:
      *
      * TNServer*DefaultTransaction:            TSIG
      *

      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      ***************
      * "CONSTANTS" *
      ***************

      **************************************************************
      *                                                            *
      * Max number of unsuccessful tries                           *
      *                                                            *
      **************************************************************
Custom 77  C-NO-OF-TRIES-ALLOWED           PIC S9(4) COMP VALUE 4.

       77  C-TN3270                        PIC X     VALUE 'T'.
       77  C-DATE-SEP                      PIC X     VALUE '/'.
       77  C-TIME-SEP                      PIC X     VALUE ':'.
       77  C-COMMAS                        PIC X     VALUE ','.
       77  C-TIME-INTERVAL                 PIC S9(7) COMP-3 VALUE 1.
       77  C-FN-SIGNON                     PIC X(2)  VALUE X"7402".
       77  C-FN-SIGNOFF                    PIC X(2)  VALUE X"7404".
       77  C-TRUE                          PIC X     VALUE 'Y'.
       77  C-FALSE                         PIC X     VALUE 'N'.
       77  C-TCT-ENTRY-NOT-FOUND           PIC S9(8) COMP VALUE -1.
       77  C-TCT-INDEX                     PIC S9(8) COMP VALUE ZERO.
       77  C-ASKIP-DRK                     PIC X     VALUE '@'.
DFSDFn 77  C-SIGN-MAP-NAME                 PIC X(8)  VALUE SPACES.
DFSDFn 77  C-MAP-SET-NAME                  PIC X(8)  VALUE SPACES.
       77  C-SIGN-ON-OPERATOR              PIC X(26)
                                 VALUE '94N                   #  N'.
      *not used as default. contained in commented out code.
       77  C-APPLICATION-TRANSID           PIC X(4)  VALUE 'XXXX'.
      * EIBRESP2 values
       77  C-TERM-ID-OK                    PIC S9(4) COMP VALUE ZERO.
       77  C-SIGNON-SUCCESSFUL             PIC S9(4) COMP VALUE ZERO.
       77  C-INVALID-PASSWORD              PIC S9(4) COMP VALUE 2.
       77  C-PASSWORD-EXPIRED              PIC S9(4) COMP VALUE 3.
       77  C-INVALID-USERID                PIC S9(4) COMP VALUE 8.
       77  C-PASSWORD-SUSPENDED            PIC S9(4) COMP VALUE 19.
      * made new eibresp2 codes
       77  C-SIGNON-CANCELLED              PIC S9(4) COMP VALUE 503.
       77  C-INVALID-PARAMETER             PIC S9(4) COMP VALUE 504.
       77  C-TRAN-ABENDED                  PIC S9(4) COMP VALUE 505.
       77  C-UNKNOWN-ERROR                 PIC S9(4) COMP VALUE 506.
       77  C-OPERATOR-SUCCESSFUL           PIC S9(4) COMP VALUE 507.
       77  C-OPERATOR-NAME-INVALID         PIC S9(4) COMP VALUE 508.
       77  C-OPERATOR-PASSWORD-INVALID     PIC S9(4) COMP VALUE 509.
       77  C-CPLT-MODE                     PIC X          VALUE 'C'.
       77  C-SIGN-MODE                     PIC X          VALUE 'S'.

      *********************
      * boolean variables *
      *********************

      **************************************************************
      * set DISCONNECT-ENABLED  to 'N' if you do not want the      *
      * terminal to logoff when you press CLEAR or after N no. of  *
      * unsuccessful tries.                                        *
      * Where N being the variable C-NO-OF-TRIES-ALLOWED           *
      **************************************************************
Custom 01  DISCONNECT-ENABLED              PIC X     VALUE 'Y'.
           88 B-DISCONNECT-ENABLED                   VALUE 'Y'.
           88 B-DISCONNECT-DISABLED                  VALUE 'N'.

       01  TRAN-MODE                       PIC X     VALUE 'C'.
           88 B-CPLT-MODE                            VALUE 'C'.
           88 B-SIGN-MODE                            VALUE 'S'.

      **********************
      * standard variables *
      **********************
       01  WS-USERI              PIC X(8) VALUES SPACES.
       01  WS-PASSI              PIC X(8) VALUES SPACES.
       01  WS-NPASSI             PIC X(8) VALUES SPACES.
       01  START-CODE.
           05 START-CODE-1       PIC X.
           05 START-CODE-2       PIC X.
       01  CURSOR-POS            PIC S9(4) BINARY VALUE ZERO.
       01  ABSTIME                         PIC S9(15) COMP-3 VALUE ZERO.
       01  LINE-BUFFER.
           05 TRAN-ID            PIC X(04) VALUE SPACES.
           05 TRAN-ID-N-CHAR     PIC X(01) VALUE SPACES.
           05 SEPARATOR          PIC X(01) VALUE SPACE.
           05 SIGN-PARM          PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(70) VALUE SPACES.
       01  KIX-DATE              PIC X(08) VALUE SPACES.
       01  LOG-DATE              REDEFINES KIX-DATE.
           05 LOG-MT             PIC X(02).
           05 FILLER             PIC X(01).
           05 LOG-DD             PIC X(02).
           05 FILLER             PIC X(01).
           05 LOG-YY             PIC X(02).
           05 FILLER             PIC X(01).
            
       01  KIX-TIME              PIC X(8)  VALUE SPACES.
       01  LOG-TIME              REDEFINES KIX-TIME.
           05 LOG-HH             PIC X(02).
           05 FILLER             PIC X(01).
           05 LOG-MN             PIC X(02).
           05 FILLER             PIC X(01).
           05 LOG-SS             PIC X(02).
           05 FILLER             PIC X(01).
       
       01  LOOP-CTR              PIC S9(4) COMP VALUE ZERO.
       01  EIB-FUNCTION          PIC X(2)       VALUE SPACES.
       01  EIB-FUNCTION-COMP     PIC 9(4)  COMP REDEFINES EIB-FUNCTION.
       01  WS-USERNAME           PIC X(20) VALUE SPACES.
       01  WS-USER-ID            PIC X(08) VALUE SPACES.
       01  WS-PREV-USER-ID       PIC X(08) VALUE SPACES.

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

       01  UIMS812E.
           05  FILLER          PIC X(9)  VALUE 'UIMS812E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(8)  VALUE ' Userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(15) VALUE ' not recognized'.

       01  UIMS811E.
           05  FILLER          PIC X(9)  VALUE 'UIMS811E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(29)
                          VALUE ' Password invalid for userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.

       01  UIMS800I.
      *    first byte of the first message line must be blank
           05  FILLER          PIC X(01) VALUE ' '.
           05  FILLER          PIC X(09) VALUE 'UIMS800I '.
           05  USER-ID         PIC X(08) VALUE SPACES.
           05  FILLER          PIC X(12) VALUE ' Last-Used '.
           05  N-DAYS          PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(01) VALUE ' '.
           05  N-MONTH         PIC X(03) VALUE SPACES.
           05  FILLER          PIC X(01) VALUE ' '.
           05  N-YEARS         PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(01) VALUE ' '.
           05  N-HOURS         PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(01) VALUE ':'.
           05  N-MINS          PIC X(02) VALUE SPACES.
           05  FILLER          PIC X(08) VALUE ' System='.
           05  SYS-ID          PIC X(04) VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Facility='.
           05  APPL-ID         PIC X(08) VALUE SPACES.

       01  UIMS801I.
      *    first byte of the first message line must be blank
           05  FILLER          PIC X(01) VALUE ' '.
           05  FILLER          PIC X(09) VALUE 'UIMS801I '.
           05  FILLER          PIC X(06) VALUE 'Count='.
           05  N-COUNT         PIC X(05) VALUE SPACES.
           05  FILLER          PIC X(06) VALUE ' Mode='.
           05  N-MODE          PIC X(04) VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' LockTime='.
           05  N-LOCKTIME      PIC X(04) VALUE SPACES.
           05  FILLER          PIC X(06) VALUE ' Name='.
           05  N-USERNAME      PIC X(20) VALUE SPACES.

       01  UIMS802I.
      *    first byte of the first message line must be blank
           05  FILLER          PIC X(01) VALUE ' '.
           05  FILLER          PIC X(09) VALUE 'UIMS802I '.
           05  FILLER          PIC X(20) VALUE 'Sign Off Transaction'.
           05  FILLER          PIC X(13) VALUE ' Successfully'.
           05  FILLER          PIC X(11) VALUE ' Terminated'.

       01  UIMS803I.
           05  FILLER          PIC X(9)  VALUE 'UIMS803I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(20) VALUE ' Sign On Transaction'.
           05  FILLER          PIC X(10) VALUE ' Cancelled'.

       01  UIMS490E.
           05  FILLER          PIC X(9)  VALUE 'UIMS490E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(15) VALUE ' Unknown error '.
           05  EIB-RESP-2      PIC 9(9)  VALUE ZERO.
           05  FILLER          PIC X(16) VALUE ' in transaction '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.

       01  UIMS494S.
           05  FILLER          PIC X(9)  VALUE 'UIMS494S '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(6)  VALUE ' Tran '.
           05  TRAN-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(18) VALUE ' abended: EIBFN = '.
           05  EIB-FN          PIC 9(5)  VALUE ZERO.

       01  UIMS495E.
           05  FILLER          PIC X(9)  VALUE 'UIMS495E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(7)  VALUE ' Error '.
           05  INV-PARM        PIC X(3)  VALUE SPACES.
           05  FILLER          PIC X(23) VALUE ' is an invalid option.'.
           05  FILLER          PIC X(7)  VALUE ' Usage '.
           05  USAGE-TEXT      PIC X(50) VALUE '/SIGN [ON|OFF]'.

       01  UIMS496E.
           05  FILLER          PIC X(9)  VALUE 'UIMS496E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(29)
                  VALUE ' Password expired for Userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(17)
                  VALUE ' - must change PW'.

       01  UIMS498E.
           05  FILLER          PIC X(9)  VALUE 'UIMS498E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(30)
                  VALUE ' Password disabled for userid '.
           05  USER-ID         PIC X(8)  VALUE SPACES.

       01  UIMS496I.
           05  FILLER          PIC X(9)  VALUE 'UIMS496I '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(31)
                         VALUE ' is already logged on elsewhere'.

       01  UIMS497E.
           05  FILLER          PIC X(9)  VALUE 'UIMS497E '.
           05  APPL-ID         PIC X(8)  VALUE SPACES.
           05  FILLER          PIC X(10) VALUE ' Terminal '.
           05  TERM-ID         PIC X(4)  VALUE SPACES.
           05  FILLER          PIC X(40)
                     VALUE ' is already signed on, you must sign off'.


      *************
      * copybooks *
      *************
       COPY DFSDF1.
       COPY DFSDF2.

       COPY DFHAID.

      ***********************
      * communications area *
      ***********************
       01  COMM-AREA.
           05  COMM-SCRNWD            PIC S9(4) COMP  VALUE ZERO.
           05  COMM-NO-OF-TRIES       PIC S9(4) COMP  VALUE ZERO.
           05  COMM-APPL-ID           PIC  X(8)       VALUE SPACES.
           05  COMM-SYS-ID            PIC  X(4)       VALUE SPACES.
           05  COMM-FAC-ID            PIC  X(8)       VALUE SPACES.

      ***************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA                   PIC X(24).

      ********
      * main *
      ********
       PROCEDURE DIVISION.

       MAIN SECTION.
       IF EIBTRNID = '/SIG'
       THEN
         MOVE C-SIGN-MODE TO TRAN-MODE
DFSDF1   MOVE 'DFSDF1'    TO C-MAP-SET-NAME
DFSDF1   MOVE 'SIGNDP2'   TO C-SIGN-MAP-NAME
       ELSE
         MOVE C-CPLT-MODE TO TRAN-MODE
DFSDF2   MOVE 'DFSDF2'    TO C-MAP-SET-NAME
DFSDF2   MOVE 'PAGS2'     TO C-SIGN-MAP-NAME
       END-IF.

      ******************************************
      * do transaction specific initialization *
      ******************************************
           PERFORM INITIALIZATION.

      ********************************************
      * if start of logical transaction          *
      *    do first time specific initialization *
      *       exit with return tranid            *
      * else                                     *
      *    restore communications area           *
      *    receive data from screen.             *
      ********************************************
           IF EIBCALEN = ZERO
              PERFORM FIRST-TIME-IN
              GO TO EXIT-RETURN-TRAN-ID 
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
      * try to sign on                  *
      * return value: EIBRESP2          *
      ***********************************
              PERFORM VALIDATE-SIGNON

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
                 PERFORM DISPLAY-SIGN-ON-MESSAGE
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


       EXIT-LOGOFF-TERMINAL.

           IF B-DISCONNECT-ENABLED
              PERFORM LOGOFF-TERMINAL.

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
      ***/| Execute /DSC to logoff current terminal|
      * / |                                        |
      * | |  input(s): none                        |
      * | | output(s): none                        |
      * | +----------------------------------------+
      * |/                                        /
      * /________________________________________/*
      ********************************************* 
       LOGOFF-TERMINAL SECTION.

           EXEC CICS RETURN TRANSID('/DSC') IMMEDIATE
           END-EXEC.

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
      * | | output(s): DATEO                       |
      * | |          : HOURO                       |
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

           IF B-SIGN-MODE
           THEN
DFSDF1       MOVE KIX-DATE TO DATEO   OF SIGNDP2O
DFSDF1       MOVE KIX-TIME TO HOURO   OF SIGNDP2O
           ELSE
DFSDF2       MOVE KIX-DATE TO DATEO   OF PAGS2O
DFSDF2       MOVE KIX-TIME TO HOURO   OF PAGS2O
DFSDF2       MOVE EIBTRMID TO OUTL04O OF PAGS2O
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
      * | |          : USERI                       |
      * | |          : EIB-FUNCTION-COMP           |
      * | | output(s): MSG                         |
      * | +----------------------------------------+
      * |/                                        / 
      * /________________________________________/* 
      ********************************************* 
       PROCESS-ERROR-CODE SECTION.

           IF B-SIGN-MODE
           THEN
DFSDF1       MOVE USERI OF SIGNDP2I TO WS-USERI
           ELSE
DFSDF2       MOVE USERI OF PAGS2I   TO WS-USERI
           END-IF.

           EVALUATE EIBRESP2

               WHEN C-INVALID-PASSWORD
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS811E
                       MOVE WS-USERI TO USER-ID OF UIMS811E
                       MOVE UIMS811E TO MSG

               WHEN C-INVALID-USERID
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS812E
                       MOVE WS-USERI TO USER-ID OF UIMS812E
                       MOVE UIMS812E TO MSG

               WHEN C-PASSWORD-EXPIRED
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS496E
                       MOVE WS-USERI TO USER-ID OF UIMS496E
                       MOVE UIMS496E TO MSG

               WHEN C-PASSWORD-SUSPENDED
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS498E
                       MOVE WS-USERI TO USER-ID OF UIMS498E
                       MOVE UIMS498E TO MSG

               WHEN C-SIGNON-CANCELLED
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS803I
                       MOVE UIMS803I TO MSG

               WHEN C-INVALID-PARAMETER
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS495E
                       MOVE UIMS495E TO MSG

               WHEN C-TRAN-ABENDED
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS494S
                       MOVE EIBTRNID TO TRAN-ID OF UIMS494S
                       MOVE EIB-FUNCTION-COMP TO EIB-FN OF UIMS494S
                       MOVE UIMS494S TO MSG

               WHEN OTHER
                       MOVE COMM-APPL-ID TO APPL-ID OF UIMS490E
                       MOVE EIBRESP2 TO EIB-RESP-2 OF UIMS490E
                       MOVE EIBTRNID TO TRAN-ID OF UIMS490E
                       MOVE UIMS490E TO MSG

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
           IF B-SIGN-MODE
           THEN
DFSDF1       MOVE MSG TO ERRORO OF SIGNDP2O
           ELSE
DFSDF2       MOVE MSG TO OUTL02O OF PAGS2O
           END-IF.
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
      ***/| DISPLAY-SIGN-ON-MESSAGE                             |
      * / |                                                     |
      * | |                                                     |
      * | |                                                     |
      * | |  input(s):                                          |         
      * | | output(s):                                          |         
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-SIGN-ON-MESSAGE SECTION.

           EXEC CICS SEND CONTROL 
                          ERASE 
                          NOHANDLE
           END-EXEC.

           ADD COMM-SCRNWD TO CURSOR-POS.
           ADD COMM-SCRNWD TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.

Custom* Actual Data/Time of Last Log should be retrieved from Security DB
Custom     EXEC CICS
Custom         ASKTIME ABSTIME(ABSTIME)
Custom     END-EXEC.
Custom     EXEC CICS FORMATTIME ABSTIME(ABSTIME)
Custom                          DATESEP(C-DATE-SEP) MMDDYY(LOG-DATE)
Custom                          TIMESEP(C-TIME-SEP) TIME(LOG-TIME)
Custom     END-EXEC.
Custom
Custom* Actual values should be retrieved from Security DB
Custom     MOVE '55555' TO N-COUNT      OF UIMS801I.
Custom     MOVE 'Warn'  TO N-MODE       OF UIMS801I.
Custom     MOVE 'None'  TO N-LOCKTIME   OF UIMS801I.

           EVALUATE LOG-MT
             WHEN '01'  MOVE 'Jan' TO N-MONTH OF UIMS800I
             WHEN '02'  MOVE 'Feb' TO N-MONTH OF UIMS800I
             WHEN '03'  MOVE 'Mar' TO N-MONTH OF UIMS800I
             WHEN '04'  MOVE 'Apr' TO N-MONTH OF UIMS800I
             WHEN '05'  MOVE 'May' TO N-MONTH OF UIMS800I
             WHEN '06'  MOVE 'Jun' TO N-MONTH OF UIMS800I
             WHEN '07'  MOVE 'Jul' TO N-MONTH OF UIMS800I
             WHEN '08'  MOVE 'Aug' TO N-MONTH OF UIMS800I
             WHEN '09'  MOVE 'Sep' TO N-MONTH OF UIMS800I
             WHEN '10'  MOVE 'Oct' TO N-MONTH OF UIMS800I
             WHEN '11'  MOVE 'Nov' TO N-MONTH OF UIMS800I
             WHEN '12'  MOVE 'Dec' TO N-MONTH OF UIMS800I
             WHEN OTHER MOVE '???' TO N-MONTH OF UIMS800I
           END-EVALUATE.

           EXEC CICS ASSIGN USERNAME(WS-USERNAME) 
                            USERID(WS-USER-ID)
           END-EXEC.

           MOVE LOG-YY       TO N-YEARS OF UIMS800I.
           MOVE LOG-DD       TO N-DAYS  OF UIMS800I.
           MOVE LOG-HH       TO N-HOURS OF UIMS800I.
           MOVE LOG-MN       TO N-MINS  OF UIMS800I.
           MOVE WS-USER-ID   TO USER-ID OF UIMS800I.
           MOVE COMM-SYS-ID  TO SYS-ID  OF UIMS800I.
           MOVE COMM-APPL-ID TO APPL-ID OF UIMS800I.
           MOVE UIMS800I TO MSG.

           EXEC CICS SEND
                     FROM(MSG)
                     LENGTH(LENGTH OF MSG)
           END-EXEC.

           ADD COMM-SCRNWD TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.

           EXEC CICS ASSIGN USERNAME(WS-USERNAME) 
           END-EXEC.
           MOVE WS-USERNAME TO N-USERNAME OF UIMS801I.
           MOVE UIMS801I TO MSG.

           EXEC CICS SEND
                     FROM(MSG)
                     LENGTH(LENGTH OF MSG)
           END-EXEC.

           MOVE 0 TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          FREEKB
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.
       DISPLAY-SIGN-ON-MESSAGE-EXIT.

           EXIT.

      *   +-----------------------------------------------------+
      ***/| DISPLAY-SIGN-OFF-MESSAGE                            |
      * / |                                                     |
      * | |                                                     |
      * | |                                                     |
      * | |  input(s):                                          |         
      * | | output(s):                                          |         
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-SIGN-OFF-MESSAGE SECTION.

           EXEC CICS SEND CONTROL 
                          ERASE 
                          NOHANDLE
           END-EXEC.

           ADD COMM-SCRNWD TO CURSOR-POS.
           ADD COMM-SCRNWD TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.

Custom* Actual Data/Time of Last Log should be retrieved from Security DB
Custom     EXEC CICS
Custom         ASKTIME ABSTIME(ABSTIME)
Custom     END-EXEC.
Custom     EXEC CICS FORMATTIME ABSTIME(ABSTIME)
Custom                          DATESEP(C-DATE-SEP) MMDDYY(LOG-DATE)
Custom                          TIMESEP(C-TIME-SEP) TIME(LOG-TIME)
Custom     END-EXEC.
Custom

           EVALUATE LOG-MT
             WHEN '01'  MOVE 'Jan' TO N-MONTH OF UIMS800I
             WHEN '02'  MOVE 'Feb' TO N-MONTH OF UIMS800I
             WHEN '03'  MOVE 'Mar' TO N-MONTH OF UIMS800I
             WHEN '04'  MOVE 'Apr' TO N-MONTH OF UIMS800I
             WHEN '05'  MOVE 'May' TO N-MONTH OF UIMS800I
             WHEN '06'  MOVE 'Jun' TO N-MONTH OF UIMS800I
             WHEN '07'  MOVE 'Jul' TO N-MONTH OF UIMS800I
             WHEN '08'  MOVE 'Aug' TO N-MONTH OF UIMS800I
             WHEN '09'  MOVE 'Sep' TO N-MONTH OF UIMS800I
             WHEN '10'  MOVE 'Oct' TO N-MONTH OF UIMS800I
             WHEN '11'  MOVE 'Nov' TO N-MONTH OF UIMS800I
             WHEN '12'  MOVE 'Dec' TO N-MONTH OF UIMS800I
             WHEN OTHER MOVE '???' TO N-MONTH OF UIMS800I
           END-EVALUATE.

           MOVE LOG-YY          TO N-YEARS OF UIMS800I.
           MOVE LOG-DD          TO N-DAYS  OF UIMS800I.
           MOVE LOG-HH          TO N-HOURS OF UIMS800I.
           MOVE LOG-MN          TO N-MINS  OF UIMS800I.
           MOVE WS-PREV-USER-ID TO USER-ID OF UIMS800I.
           MOVE COMM-SYS-ID     TO SYS-ID  OF UIMS800I.
           MOVE COMM-APPL-ID    TO APPL-ID OF UIMS800I.
           MOVE UIMS800I TO MSG.

           EXEC CICS SEND
                     FROM(MSG)
                     LENGTH(LENGTH OF MSG)
           END-EXEC.

           ADD COMM-SCRNWD TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.

           MOVE UIMS802I TO MSG.

           EXEC CICS SEND
                     FROM(MSG)
                     LENGTH(LENGTH OF MSG)
           END-EXEC.

           MOVE 0 TO CURSOR-POS.
           EXEC CICS SEND CONTROL 
                          FREEKB
                          CURSOR(CURSOR-POS)
                          NOHANDLE
           END-EXEC.
       DISPLAY-SIGN-OFF-MESSAGE-EXIT.

           EXIT.
      *   +-----------------------------------------------------+
      ***/| do first time specific initialization once per      |
      * / | logical transaction.                                |
      * | |                                                     |
      * | |  input(s): none                                     |
      * | | output(s): COMM-NO-OF-TRIES                         |
      * | |          : COMM-APPL-ID                             |
      * | |          : EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       FIRST-TIME-IN SECTION.

           EXEC CICS ASSIGN APPLID(COMM-APPL-ID) 
                            SYSID(COMM-SYS-ID)
                            FACILITY(COMM-FAC-ID)
                            SCRNWD(COMM-SCRNWD)
                            STARTCODE(START-CODE)
                            USERID(WS-PREV-USER-ID)
           END-EXEC.
           PERFORM SIGN-OFF-USER.

      * receive command line into line-buffer
         IF START-CODE-1 = 'T'
           EXEC CICS RECEIVE
                     INTO(LINE-BUFFER)
                     LENGTH(LENGTH OF LINE-BUFFER)
                     ASIS
           END-EXEC
         ELSE
           EXEC CICS RETRIEVE
                     INTO(LINE-BUFFER)
                     LENGTH(LENGTH OF LINE-BUFFER)
           END-EXEC
         END-IF.

      **********************************
      * if parameters were passed      *
      *    process the parameters      *
      * else                           *
      *    transaction will send a map *
      **********************************
           EVALUATE SIGN-PARM 
             WHEN 'ON'
             WHEN 'on'
             WHEN 'UIM'
             WHEN SPACES 
                  PERFORM FIRST-TIME-IN-MAP

             WHEN 'OFF' 
             WHEN 'off' 
                  PERFORM SIGN-OFF-USER
                  PERFORM DISPLAY-SIGN-OFF-MESSAGE
                  GO TO SUCCESSFUL-EXIT-RETURN

             WHEN OTHER
                  MOVE C-INVALID-PARAMETER TO EIBRESP2
                  MOVE SIGN-PARM TO INV-PARM OF UIMS495E
                  PERFORM PROCESS-ERROR-CODE
                  PERFORM DISPLAY-TEXT-MESSAGE
                  EXEC CICS RETURN END-EXEC
           END-EVALUATE.

       FIRST-TIME-IN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| try and sign on the user and then update the term id|
      * | |                                                     |
      * | |  input(s): USERI                                    |
      * | |          : PASSI                                    |
      * | |          : NPASSI                                   |
      * | |          : COMM-NO-OF-TRIES                         |
      * | | output(s): EIBRESP2                                 |
      * | |          : COMM-NO-OF-TRIES                         |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-ON-USER SECTION.

           ADD 1 TO COMM-NO-OF-TRIES.
           PERFORM SIGN-ON-USER-SIGN.

       SIGN-ON-USER-EXIT.

           EXIT.

      *   +-----------------------------------------------------+
      ***/| try and sign on the user using /sig                 |
      * | |                                                     |
      * | |  input(s): USERI                                    |
      * | |          : PASSI                                    |
      * | |          : NPASSI                                   |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       SIGN-ON-USER-SIGN SECTION.

      *************************************************************        
      * at this time IGNORE condition is not supported for SIGNON *
      * but the handle abend will catch any condition and return  *
      * to the paragraph SIGN-ON-USER-EXIT via the abend routine  *
      * called ABEND-ROUTINE.                                     *
      *************************************************************
           EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE) END-EXEC.
           IF B-SIGN-MODE
           THEN
DFSDF1       MOVE USERI  OF SIGNDP2I TO WS-USERI
DFSDF1       MOVE PASSI  OF SIGNDP2I TO WS-PASSI
DFSDF1       MOVE NPASSI OF SIGNDP2I TO WS-NPASSI
           ELSE
DFSDF2       MOVE USERI  OF PAGS2I   TO WS-USERI
DFSDF2       MOVE PASSI  OF PAGS2I   TO WS-PASSI
DFSDF2       MOVE NPASSI OF PAGS2I   TO WS-NPASSI
           END-IF.
           IF WS-NPASSI = SPACES OR WS-NPASSI = LOW-VALUES
              EXEC CICS SIGNON
                        USERID(WS-USERI)
                        PASSWORD(WS-PASSI)
              END-EXEC
           ELSE
              EXEC CICS SIGNON
                        USERID(WS-USERI)
                        PASSWORD(WS-PASSI)
                        NEWPASSWORD(WS-NPASSI)
              END-EXEC 
           END-IF.

       SIGN-ON-USER-SIGN-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| do first time specific initialization once per      |
      * / | logical transaction.                                |
      * | |                                                     |
      * | |  input(s): none                                     |
      * | | output(s): COMM-NO-OF-TRIES                         |
      * | |          : COMM-APPL-ID                             |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       FIRST-TIME-IN-MAP SECTION.

      * about to attempt first try
           MOVE 1 TO COMM-NO-OF-TRIES.
      * display initial map screen
           PERFORM DISPLAY-SCREEN.

       FIRST-TIME-IN-MAP-EXIT.

           EXIT.


      *   +-----------------------------------------------------+
      ***/| display map screen.                                 |
      * | |                                                     |
      * | |  input(s): PAGS2O                                   |
      * | | output(s): displayed map screen                     |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       DISPLAY-SCREEN SECTION.

      * populate map with current date and time.
           PERFORM GET-DATE-TIME.

           IF B-SIGN-MODE
           THEN
DFSDF1       MOVE SPACES TO USERO  OF SIGNDP2O
DFSDF1       MOVE SPACES TO PASSO  OF SIGNDP2O
DFSDF1       MOVE SPACES TO NPASSO OF SIGNDP2O
             EXEC CICS
                SEND MAP (C-SIGN-MAP-NAME)
                     MAPSET(C-MAP-SET-NAME)
DFSDF1               FROM(SIGNDP2O)
DFSDF1               LENGTH(LENGTH OF SIGNDP2O)
                     ERASE
                     FREEKB
             END-EXEC
           ELSE
DFSDF2       MOVE SPACES TO USERO  OF PAGS2O
DFSDF2       MOVE SPACES TO PASSO  OF PAGS2O
DFSDF2       MOVE SPACES TO NPASSO OF PAGS2O
             EXEC CICS
                SEND MAP (C-SIGN-MAP-NAME)
                     MAPSET(C-MAP-SET-NAME)
DFSDF2               FROM(PAGS2O)
DFSDF2               LENGTH(LENGTH OF PAGS2O)
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
      * | | output(s): PAGS2I                                   |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       ACCEPT-SCREEN-INPUT SECTION.

           IF B-SIGN-MODE
           THEN
             EXEC CICS
                RECEIVE MAP (C-SIGN-MAP-NAME)
                        MAPSET(C-MAP-SET-NAME)
DFSDF1                  INTO(SIGNDP2I)
                        ASIS
             END-EXEC
           ELSE
             EXEC CICS
                RECEIVE MAP (C-SIGN-MAP-NAME)
                        MAPSET(C-MAP-SET-NAME)
DFSDF2                  INTO(PAGS2I)
                        ASIS
             END-EXEC
           END-IF.

       ACCEPT-SCREEN-INPUT-EXIT.

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
      * | |  input(s): USERI                                    |
      * | |          : PASSI                                    |
      * | |          : NPASSI                                   |
      * | | output(s): EIBRESP2                                 |
      * | +-----------------------------------------------------+
      * |/                                                     / 
      * /_____________________________________________________/* 
      ********************************************************** 
       VALIDATE-SIGNON SECTION.

           PERFORM SIGN-ON-USER.

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
                   GO TO SIGN-ON-USER-SIGN-EXIT

              WHEN C-FN-SIGNOFF
                   GO TO SIGN-OFF-USER-EXIT

              WHEN OTHER
                   EXEC CICS HANDLE ABEND END-EXEC
                   MOVE EIBFN TO EIB-FUNCTION
                   MOVE C-TRAN-ABENDED TO EIBRESP2
                   PERFORM DISPLAY-TEXT-MESSAGE
                   EXEC CICS ABEND END-EXEC

           END-EVALUATE.
