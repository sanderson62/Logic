      *****************************************************************
      *                                                               *
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
	IDENTIFICATION DIVISION.
	PROGRAM-ID. KXALT016.
	ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
	    SELECT DATAFILE ASSIGN TO EXTERNAL ALTFILE
			  ORGANIZATION IS RECORD SEQUENTIAL
			  ACCESS MODE IS SEQUENTIAL
			  LOCK MODE IS MANUAL
			  STATUS IS SORT-STATUS.

	    SELECT SRTFILE ASSIGN TO EXTERNAL SORTFILE
	        SORT STATUS IS SORT-STATUS.

	DATA DIVISION.
	FILE SECTION.

	FD  DATAFILE.
	01  DATA-REC			PIC X(16).

	SD  SRTFILE.
	01  SORT-REC.
	    05	SORT-KEY		PIC X(16).

	WORKING-STORAGE	SECTION.
	01 SORT-STATUS        PIC XX.
	01 SORT-STATUS-99     REDEFINES SORT-STATUS
			      PIC 99.
	01 SORT-STATUS-BY     REDEFINES SORT-STATUS.
	   05 STAT-BY1        PIC X.
	   05 STAT-BY2        PIC X.
	01 STAT-BIN           REDEFINES SORT-STATUS
			      PIC S9(4) COMP.
	PROCEDURE DIVISION.
      *
      * case 525 - Implement some error handling logic in case
      *            SORT falls over for some reason. Set the return
      *            code to the runtime error and pass back to
      *            kxaltsort shell script.
      *
	DECLARATIVES.
	SORT-ERR SECTION.
	    USE AFTER STANDARD ERROR PROCEDURE ON DATAFILE SRTFILE.
	END DECLARATIVES.
	0000-MAIN SECTION.

	    MOVE "00" TO SORT-STATUS.
	    SET RETURN-CODE TO 0.
	    SORT SRTFILE ON ASCENDING KEY SORT-KEY
		 USING DATAFILE
		 GIVING	DATAFILE.
	    IF STAT-BY1 = "9"
		MOVE LOW-VALUES TO STAT-BY1
		SET RETURN-CODE TO 99
		DISPLAY "kxalt016: STATUS CODE 9/" STAT-BIN
	    ELSE
	    IF SORT-STATUS NOT EQUAL "00"
		SET RETURN-CODE TO SORT-STATUS-99 
		DISPLAY "kxalt016: STATUS CODE " SORT-STATUS-99
	    ELSE
		DISPLAY "kxalt016: STATUS CODE 00".
	    STOP RUN.

