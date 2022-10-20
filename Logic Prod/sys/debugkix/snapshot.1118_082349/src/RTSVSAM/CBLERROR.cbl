      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           call "kxcobol_trace_dump".
           call "CCFerrorwrap".
           move 1 to RETURN-CODE.
	     exit program.
