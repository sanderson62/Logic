      ****************************************************************
      *                                                              *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                         *
      *                                                              *
      ****************************************************************
      *  The following record is updated when called by a user       *
      *  cobol program for TCT table information.                    *
      *                                                              *
      *  user code should call the 'kxtctinfo' function during start *
      *  transaction user exit to get the latest KIX-TCT-INFO record *
      *  updated.                                                    * 
      *                                                              *
      *  example:-  call "kxtctinfo" using KIX-TCT-INFO              *
      ****************************************************************

       01  KIX-TCT-INFO.
	   05  KIX-USRNAM                 PIC X(8).
	   05  KIX-OPSEC                  PIC X(8).
	   05  KIX-OPCLS                  PIC X(3).
	   05  KIX-TRMID                  PIC X(4).
	   05  KIX-LUNAME                 PIC X(8).
	   05  KIX-OPID                   PIC X(3).
	   05  KIX-TRANCD                 PIC X(4).

      ****************************************************************
      *  The following record points to the application name from the*
      *  sit table.                                                  *
      *                                                              *
      *  user code should call the 'kxsysinfo' function during       *
      *  allocate transaction  user exit to get the KIX-SYS-INFO     *
      *  record updated.                                             * 
      *                                                              *
      *  example:-  call "kxsysinfo" using KIX-SYS-INFO              *
      ****************************************************************
       01  KIX-SYS-INFO.
	   05  KIX-SITNAME                PIC X(8).
	   05  KIX-SIT-SRV-NAME           PIC X(8).
	   05  KIX-SIT-DB-NAME            PIC X(8).
	   05  KIX-SIT-USR-NAME           PIC X(8).
	   05  KIX-SIT-USR-PASS           PIC X(8).
	   05  KIX-TRANIDX                PIC X(3).

      ****************************************************************
      * User exit sets the user message and displays on user terminal*
      *                                                              *
      *  user code should call the 'kxsetmsg' any time in the program*
      *  to put the message on the user terminal.                    *
      *                                                              *
      *  example:-  call "kxsetmsg" using KIX-MSG-INFO               *
      ****************************************************************
       01  KIX-MSG-INFO.
	   05  KIX-MSGNO                 PIC 9(4)
	       VALUE 1098.
	   05  KIX-MSG-ROUTINE           PIC X(20).
	   05  KIX-MSG-STR               PIC X(60).
      ****************************************************************
      *    Do not remove this (reqd. for satisfying C calls          *
      ****************************************************************
	   05  FILLER                    PIC X VALUE x"00".

      ****************************************************************
      *  User exit sets the UNIX tty information for the terminal    *
      *                                                              *
      *  user code should call the 'kxttyinfo' any time in the program*
      *  to get the UNIX tty information.                            *
      *                                                              *
      *  example:-  call "kxttyinfo" using KIX-TTY-INFO               *
      ****************************************************************
       01  KIX-TTY-INFO.
	   05  KIX-TTY-PID               PIC 9(6).
	   05  KIX-TTY-NAME              PIC X(20).
	   05  KIX-TTY-TERMENV           PIC X(14).
      ****************************************************************
      *    Do not remove this (reqd. for satisfying C calls          *
      ****************************************************************
	   05  FILLER                    PIC X VALUE x"00".
