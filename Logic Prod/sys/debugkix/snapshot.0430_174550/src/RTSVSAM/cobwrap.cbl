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
       01  EXIT-INSTALL-PARMS.
           02 EXIT-INSTALL-ADDRS USAGE IS PROCEDURE-POINTER.
           02 EXIT-INSTALL-PRRTY PIC X COMP-X VALUE 0.
       01  INSTALL-STATUS-CODE PIC 9(4) COMP VALUE ZEROS.
       01  FUNC-CODE PIC X COMP-X VALUE 18.
       01  PARAMETER PIC X COMP-X VALUE 1.
       LINKAGE SECTION.
       01  PROGLEN  PIC X(4) COMP-5.
      * PROGNAME PIC X(40) is tied to CPR_start_pgm[MAXAP_NAME]
      * in CPR.c
       01  PROGNAME PIC X(40).
       01  MESSAGE-STORAGE.
           02  PARM-ARGC  PIC 9(4) COMP.
           02  PARM-ARGV.
             03 READPARM  PIC X OCCURS 1 TO 100 DEPENDING ON PARM-ARGC.
      *====================================================
       PROCEDURE DIVISION USING PROGLEN, PROGNAME.
      *
       OOO-INIT-PARAG.
           CALL "ebmMFbridge" USING VALUE 0.
           CALL "tp_catchsign".
           SET EXIT-INSTALL-ADDRS TO ENTRY "CBLEXIT".
      *    installing the exit procedure.
           CALL "CBL_EXIT_PROC" USING EXIT-INSTALL-FLAG
                 EXIT-INSTALL-PARMS RETURNING INSTALL-STATUS-CODE.
           SET EXIT-INSTALL-ADDRS TO ENTRY "CBLERROR".
      *    installing the error procedure.
           CALL "CBL_ERROR_PROC" USING EXIT-INSTALL-FLAG
                EXIT-INSTALL-PARMS RETURNING INSTALL-STATUS-CODE.
           CALL "read_parm"  USING ADDRESS OF MESSAGE-STORAGE.
           CALL PROGNAME USING  MESSAGE-STORAGE.
           MOVE RETURN-CODE TO MAIN-RETURN-CODE.
           CALL "CCFstopwrap" USING MAIN-RETURN-CODE.
           SET EXIT-INSTALL-ADDRS TO ENTRY "CBLEXIT".
      *    deinstalling the exit procedure.
           CALL "CBL_EXIT_PROC" USING EXIT-DEINSTALL-FLAG
                EXIT-INSTALL-PARMS RETURNING INSTALL-STATUS-CODE.
           SET EXIT-INSTALL-ADDRS TO ENTRY "CBLERROR".
      *    deinstalling the error procedure.
           CALL "CBL_ERROR_PROC" USING EXIT-DEINSTALL-FLAG
                EXIT-INSTALL-PARMS RETURNING INSTALL-STATUS-CODE.
           EXIT PROGRAM.
       ENTRY "CBLEXIT".
           CALL "CCFexitwrap".
           EXIT PROGRAM.
           STOP RUN.
