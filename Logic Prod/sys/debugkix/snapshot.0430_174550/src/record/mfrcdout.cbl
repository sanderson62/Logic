      $SET OSVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MFRECORDOUT.
      ***             ================
       AUTHOR.        CLERITY.
       DATE-WRITTEN.  JUNE 1991.
       DATE-COMPILED.

       REMARKS.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      * If 'Large File' (>2G) is anticipated be sure to consult the   *
      * Merant (MicroFocus) COBOL documentation on File Handling      *
      * For Server Express.                                           *
      * Especially the discussion of the:                             *
      *            EXTFH environment variable                         *
      *            FILEMAXSIZE=8 argument                             *
      *                                                               *
      *                                                               *
      *****************************************************************
      * $Workfile:   mfrcdout.cbl  $ $Revision:   1.0  $
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                CLERITY.
       OBJECT-COMPUTER.                CLERITY.
      /
       INPUT-OUTPUT SECTION.
      ******************************************************************
      ******************************************************************
      ***                                                            ***
      ***                F I L E   C O N T R O L                     ***
      ***                                                            ***
      ******************************************************************
      ******************************************************************
       FILE-CONTROL.

           SELECT  OUTREC   ASSIGN TO DYNAMIC  MFOUT
	       ORGANIZATION IS RECORD SEQUENTIAL
               FILE STATUS IS CHK.
      /
       DATA DIVISION.
      ******************************************************************
      ******************************************************************
      ***                                                            ***
      ***                 F I L E   S E C T I O N                    ***
      ***                                                            ***
      ******************************************************************
      ******************************************************************
       FILE SECTION.

       FD  OUTREC
           LABEL RECORDS ARE STANDARD
	   RECORD IS VARYING IN SIZE FROM 4 TO 32767 CHARACTERS
                         DEPENDING ON LEN
           DATA RECORD IS OUTMASTER.

       01  OUTMASTER.
           05  VARLRCD   PIC X OCCURS 4 TO 32767 DEPENDING ON LEN.
      /
      ******************************************************************
      ******************************************************************
      ***                                                            ***
      ***             W O R K I N G  -  S T O R A G E                ***
      ***                                                            ***
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  CHK                    PIC 9(02) VALUE ZEROES.
       01  CHK2 REDEFINES CHK.
           05  CHK2-BYTE1         PIC 9(01).
           05  CHK2-BYTE2         PIC 9(01).
       01  CHK2-9                 PIC 9(04) COMP VALUE ZERO.
       01  CHK2-9R REDEFINES CHK2-9.
	   05  FILLER             PIC X.
	   05  CHK2-9X            PIC 9.
       01  LEN                    PIC 9(9) COMP VALUE 4.
       01  MFOUT                  PIC X(256).

      /
      ******************************************************************
      ******************************************************************
      ***                                                            ***
      ***           P R O C E D U R E   D I V I S I O N              ***
      ***                                                            ***
      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION.

       0000-PROGRAM-ENTRY.
      *------------------*

      *** THE FOLLOWING STATEMENT IS TO GET THE NAME OF THE OUTPUT ***
      ***                  SEQUENTIAL FILE.                        *** 
           CALL "kxread" USING
	       BY REFERENCE LEN,
               BY REFERENCE OUTMASTER.
	   IF VARLRCD (LEN) < '0' THEN
	       MOVE ' ' TO VARLRCD (LEN)
	       SUBTRACT 1 FROM LEN.
	   MOVE OUTMASTER TO MFOUT.

           OPEN OUTPUT OUTREC.
           IF CHK NOT = ZERO GO TO CHKRTN.
           PERFORM READ-RECORD UNTIL RETURN-CODE NOT = ZERO.
	   MOVE 0 TO RETURN-CODE.
           STOP RUN.

      **** CALL C ROUTINE TO READ A VAR LEN RECORD FROM PIPE ****

       READ-RECORD.
           CALL "kxread" USING
               BY REFERENCE LEN,
               BY REFERENCE OUTMASTER.

	   IF RETURN-CODE = ZERO
	       PERFORM WRITE-RECORD
	   ELSE
	   IF RETURN-CODE = -1
	       PERFORM EOF-CLOSE
	   ELSE
               MOVE -3 TO CHK
	       PERFORM CHKRTN.

      **** READ SUCCESSFUL, WRITE TO FILE, IF ERROR CHECK RETURN CODE **  

       WRITE-RECORD.
	   MOVE LEN TO LEN.
	   IF LEN = 0
	       PERFORM EOF-CLOSE
	   ELSE
	       WRITE OUTMASTER
	       IF CHK NOT = 00 
		   PERFORM CHKRTN.

      **** EOF ENCOUNTERED, CLOSE OUTPUT FILE ****

       EOF-CLOSE.
           CLOSE OUTREC
           IF CHK NOT = 00 
               PERFORM CHKRTN
	   ELSE
	       MOVE ZERO TO RETURN-CODE
               STOP RUN.

      **** I/O ERROR OCCURED, RETURN ERROR CODE AND ABORT ****

       CHKRTN.
	   MOVE CHK2-BYTE2 TO CHK2-9X.
	   MOVE CHK2-9 TO RETURN-CODE.
           STOP RUN.

      ******************************************************************
      ***               E N D   O F   P R O G R A M                  ***
      ******************************************************************
