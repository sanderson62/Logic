      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID. KXESQL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      *  Code the userid and password, or use sit info to collect the
      *  application name and connect using application name.
      *---------------------------------------------------------------*
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  USERID                         PIC X(8).
       01  PASSWRD                        PIC X(30).
       01  USERNAME                       PIC X(8).
       01  SERVER                         PIC X(12).
       01  SQL-STRING                     PIC X(40).
           EXEC SQL END DECLARE SECTION END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  SQLCODE-DISP                   PIC -9(5).
      *---------------------------------------------------------------*
      * This copy book gives the tct and system information 
      *---------------------------------------------------------------*
       COPY "KXINFO.CPY".
      *---------------------------------------------------------------*
      * Sample COBOL program which contains entry points for
      * interfacing UniKix's user exits with vCOBOL ESQL software.
      * The main entry point(KXESQLLGN) is for logging onto the database
      * and is called by the UniKix transaction processors
      * and batch vsam processor when they are
      * initiated by the UniKix system. Entry point KXESQLLGF is for
      * logging off from the database and is called by the UniKix 
      * transaction processors and batch vsam processors when they
      * terminate.
      *
      * Entry point KXESQLSAVE is for executing an SQL COMMIT statement
      * and will be called at UniKix sync point time. If the user COBOL
      * programs are already doing COMMITs, then this statement will
      * have no effect.
      * Entry point KXESQLUNDO is for executing an SQL ROLLBACK
      * statement and will be called at UniKix rollback time. If the
      * user COBOL programs are already doing ROLLBACKs, then this
      * statement will have no effect.
      *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
       ENTRY "KXESQLLGN".
           MOVE "KXESQLLGN" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ABORT END-EXEC.
      *---------------------------------------------------------------*
      * This call update the sit information
      * user can use this information to set the userid . 
      * In this example, application name is used as the userid and
      * password.
      *---------------------------------------------------------------*
           CALL "kxsysinfo" using KIX-SYS-INFO.
           MOVE KIX-SIT-USR-NAME TO USERID.
           MOVE KIX-SIT-USR-PASS TO PASSWRD.
           IF KIX-SIT-SRV-NAME NOT EQUAL SPACES
              MOVE KIX-SIT-SRV-NAME TO SERVER
              EXEC SQL CONNECT       :USERID  
                       IDENTIFIED BY :PASSWRD 
                       USING         :SERVER END-EXEC
           ELSE
              EXEC SQL CONNECT       :USERID  
                       IDENTIFIED BY :PASSWRD  END-EXEC.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       ENTRY "KXESQLLGF".
           MOVE "KXESQLLGF" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ERROR END-EXEC.
           EXEC SQL COMMIT WORK END-EXEC.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       ENTRY "KXESQLBTRN".
           MOVE "KXESQLBTRN" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ABORT END-EXEC.
      *---------------------------------------------------------------*
      * This call updates the tct information
      *---------------------------------------------------------------*
           CALL "kxtctinfo" using KIX-TCT-INFO.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       ENTRY "KXESQLETRN".
           MOVE "KXESQLETRN" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ABORT END-EXEC.
           EXEC SQL COMMIT WORK END-EXEC.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       ENTRY "KXESQLSAVE".
           MOVE "KXESQLSAVE" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ERROR END-EXEC.
           EXEC SQL COMMIT WORK END-EXEC.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       ENTRY "KXESQLUNDO".
           MOVE "KXESQLUNDO" TO KIX-MSG-ROUTINE.
           EXEC SQL WHENEVER SQLERROR GOTO SQL-ABORT END-EXEC.
           EXEC SQL ROLLBACK WORK END-EXEC.
           SET RETURN-CODE TO 0.
           EXIT PROGRAM.
       SQL-ABORT.
      *---------------------------------------------------------------*
      *    Set return-code to Failure (1) or Abort (-1)
      *    Failure during allocation brings up the SERVER, without
      *           the database connected.
      *    Abort always aborts the transaction SERVER
      *    This example aborts the transaction SERVER
      *---------------------------------------------------------------*
           MOVE SQLCODE TO SQLCODE-DISP.
           MOVE SPACES TO KIX-MSG-STR.
           STRING "DATABASE ERROR. SQLCODE = ", SQLCODE-DISP
                  DELIMITED BY SIZE INTO KIX-MSG-STR.
           CALL "kxsetmsg" USING KIX-MSG-INFO.
           MOVE SPACES TO KIX-MSG-STR.
           STRING "DATABASE ERROR. ", SQLERRMC
                  DELIMITED BY SIZE INTO KIX-MSG-STR.
           CALL "kxsetmsg" USING KIX-MSG-INFO.
           SET RETURN-CODE TO -1.
           EXIT PROGRAM.
       SQL-ERROR.
      *---------------------------------------------------------------*
      *    Set return-code to Failure (1) or Abort (-1)
      *    Failure during other exits terminates the transaction
      *    Abort always aborts the transaction SERVER
      *    This example terminates transaction.
      *---------------------------------------------------------------*
           MOVE SQLCODE TO SQLCODE-DISP.
           MOVE SPACES TO KIX-MSG-STR.
           STRING "DATABASE ERROR. SQLCODE = ", SQLCODE-DISP
                  DELIMITED BY SIZE INTO KIX-MSG-STR.
           CALL "kxsetmsg" USING KIX-MSG-INFO.
           MOVE SPACES TO KIX-MSG-STR.
           STRING "DATABASE ERROR. ", SQLERRMC
                  DELIMITED BY SIZE INTO KIX-MSG-STR.
           CALL "kxsetmsg" USING KIX-MSG-INFO.
           SET RETURN-CODE TO 1.
           EXIT PROGRAM.
