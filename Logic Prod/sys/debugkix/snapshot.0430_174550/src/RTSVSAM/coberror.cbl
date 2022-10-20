      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. coberror.
       environment division.
       configuration section.
       data division.
       working-storage section.
      *====================================================
       procedure division.
      *
       entry "CBLERROR".
           call "CCFerrorwrap".
           move 1 to RETURN-CODE.
	   exit program.
