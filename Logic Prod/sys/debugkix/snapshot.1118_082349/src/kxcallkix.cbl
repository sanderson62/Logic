      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      *
      * WARNING:  It should not be necessary to change this code.
      * Changes to this file may cause unikixtran to stop working.
      *
      $SET RTNCODE-SIZE"4"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KXCALLKIX.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INSTALL-FLG    PIC X COMP-X VALUE 0.
       01  STATUS-CODE    PIC 9(4) COMP VALUE ZEROS. 
       01  INSTALL-ADDR   USAGE PROCEDURE-POINTER.
       LINKAGE SECTION.
       01  ERR-MSG        PIC X(325).       
       PROCEDURE DIVISION.
           SET INSTALL-ADDR TO ENTRY "kxCobolDump".
           CALL "CBL_ERROR_PROC" USING INSTALL-FLG INSTALL-ADDR
                                 RETURNING STATUS-CODE.
           CALL "kxdsptch" GIVING RETURN-CODE.
           EXIT PROGRAM.
