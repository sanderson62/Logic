      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. cblerror.
       environment division.
       configuration section.
       data division.
       working-storage section.
      *====================================================
       linkage section.
       01  err-msg pic x(325).
      *====================================================
       procedure division using err-msg.
      *
           DISPLAY '**ERROR** ' err-msg.
           call "CCFerrorwrap".
           move 1 to RETURN-CODE.
	     exit program.
