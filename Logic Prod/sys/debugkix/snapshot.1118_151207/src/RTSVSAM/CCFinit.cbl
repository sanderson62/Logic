      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id.                 CCFinit.
      *
      *  Cobol operation to execute at user function start time.
      *  The present module is called by the CPR (standard Runner)
      *  in the initialization phase, before to start the user program.
      *
       environment division.
       data division.
       working-storage section.
       01  work-area               pic x.
       procedure division.
       sole section.
       entry-point.
      *
      *  An "accept" is executed due to the std RTS behaviour: it does
      *  pass the command line on the first execution of an accept verb,
      *  instead of accept data from the standard input.
      *
           accept  work-area .
      *
      *  No data are received, because is the Runner, instead of the std
      *  RTS, the main entry which receives the argc/argv parameters.
      *
       exit-point.
           exit program.
      *
      *====================  end of source program  ====================  
