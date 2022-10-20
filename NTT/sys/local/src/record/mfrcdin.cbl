      $SET OSVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MFRECORDIN.
      ***             ================
       DATE-WRITTEN.  JUNE 1991.
       DATE-COMPILED.

       REMARKS.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
      * All rights reserved.                                          *
      *                                                               *
      * If 'Large File' (>2G) is anticipated be sure to consult the   *
      * Merant (MicroFocus) COBOL documentation on File Handling      *
      * For Server Express.                                           *
      * Especially the discussion of the:                             *
      *            EXTFH environment variable                         *
      *            FILEMAXSIZE=8 argument                             *
      *                                                               *
      *****************************************************************
      * $Workfile:   mfrcdin.cbl  $ $Revision:   1.2  $
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
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

           SELECT  INREC   ASSIGN TO  DYNAMIC MFIN
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

       FD  INREC
           LABEL RECORDS ARE OMITTED
	   RECORD IS VARYING IN SIZE FROM 4 TO 32767 CHARACTERS
                         DEPENDING ON LEN
           DATA RECORD IS INMASTER.

       01  INMASTER.
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
       01  LEN                    PIC 9(9)  COMP VALUE 4.
       01  ERR-CODE               PIC S9(9) COMP.
       01  EOF-FLAG               PIC X(1)  VALUE ' '.
           88  EOF-FLG            VALUE 'X'.
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

      *** THE FOLLOWING STATEMENT IS TO GET THE NAME OF THE INPUT ***
      ***                  SEQUENTIAL FILE.                       *** 
           CALL "kxread" USING
	       BY REFERENCE LEN,
               BY REFERENCE INMASTER.

	   IF VARLRCD (LEN) < '0' THEN
	       MOVE ' ' TO VARLRCD (LEN)
	       SUBTRACT 1 FROM LEN.
           MOVE INMASTER TO MFIN.

           OPEN INPUT INREC.
           IF CHK NOT = ZERO
               MOVE -2 TO ERR-CODE
               PERFORM CHKRTN.
           MOVE ZERO TO ERR-CODE.
           CALL "kxwrite" USING
               BY REFERENCE ERR-CODE,
               BY REFERENCE INMASTER.
	   IF RETURN-CODE NOT = ZERO
               MOVE RETURN-CODE TO ERR-CODE
	       PERFORM CHKRTN.
           PERFORM READ-RECORD.
           PERFORM WRT-RECORD UNTIL EOF-FLG.
           PERFORM EOF-CLOSE.
           MOVE ZERO TO RETURN-CODE.
           STOP RUN.

      **** READ A VAR LEN RECORD FROM FILE, IF ERROR RETURN ERRNO ****

       READ-RECORD.
           READ INREC INTO INMASTER
               AT END SET EOF-FLG TO TRUE.
           IF CHK NOT = 00 AND CHK NOT = 10
               MOVE -3 TO ERR-CODE
               PERFORM CHKRTN.

      **** CALL C ROUTINE TO WRITE A VAR LEN RECORD TO PIPE ****

       WRT-RECORD.
           CALL "kxwrite" USING
               BY REFERENCE LEN,
               BY REFERENCE INMASTER.
	   IF RETURN-CODE NOT = ZERO
               MOVE RETURN-CODE TO ERR-CODE
	       PERFORM CHKRTN.

           PERFORM READ-RECORD.


      **** EOF ENCOUNTERED, CLOSE OUTPUT FILE ****

       EOF-CLOSE.
           CLOSE INREC
           MOVE ZERO TO ERR-CODE
           CALL "kxwrite" USING
               BY REFERENCE ERR-CODE,
               BY REFERENCE INMASTER
           MOVE ZERO TO RETURN-CODE
           STOP RUN.

      **** I/O ERROR OCCURED, RETURN ERROR CODE AND ABORT ****

       CHKRTN.
           CALL "kxwrite" USING
               BY REFERENCE ERR-CODE,
               BY REFERENCE INMASTER
           STOP RUN.

      ******************************************************************
      ***               E N D   O F   P R O G R A M                  ***
      ******************************************************************
