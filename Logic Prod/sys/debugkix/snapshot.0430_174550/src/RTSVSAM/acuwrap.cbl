      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobwrap.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAIN-RETURN-CODE  PIC S9(4) COMP.
       01  EXIT-INSTALL-FLAG  PIC X COMP-X VALUE 0.
       01  EXIT-DEINSTALL-FLAG  PIC X COMP-X VALUE 1.
       01  INSTALL-STATUS-CODE PIC 9(4) COMP VALUE ZEROS.
       01  FUNC-CODE PIC X COMP-X VALUE 18.
       01  PARAMETER PIC X COMP-X VALUE 1.
       01  MESSAGE-STORAGE.
           02  PARM-ARGC  PIC 9(4) COMP.
           02  PARM-ARGV.
             03 READPARM  PIC X OCCURS 1 TO 100 DEPENDING ON PARM-ARGC.
       LINKAGE SECTION.
       01  PROGNAME PIC X(40).
      *====================================================
       PROCEDURE DIVISION USING PROGNAME.
      *
       OOO-INIT-PARAG.
           CALL "ebmMFbridge" USING VALUE 0.
           CALL "tp_catchsign".
      *    installing the exit procedure.
           CALL "CBL_EXIT_PROC" USING 0 "CBLEXIT"
                                 RETURNING INSTALL-STATUS-CODE.
      *    installing the error procedure.
           CALL "CBL_ERROR_PROC" USING 0 "CBLERROR"
                                 RETURNING INSTALL-STATUS-CODE.
           CALL "read_parm" USING MESSAGE-STORAGE.
           CALL PROGNAME USING  MESSAGE-STORAGE.
           MOVE RETURN-CODE TO MAIN-RETURN-CODE.
           CALL "CCFstopwrap" USING MAIN-RETURN-CODE.
      *    deinstalling the exit procedure.
           CALL "CBL_EXIT_PROC" USING 254 "CBLEXIT"
                                 RETURNING INSTALL-STATUS-CODE.
      *    deinstalling the error procedure.
           CALL "CBL_ERROR_PROC" USING 254 "CBLERROR"
                                 RETURNING INSTALL-STATUS-CODE.
           EXIT PROGRAM.
