       IDENTIFICATION DIVISION.
        PROGRAM-ID.   FNBLIST.
        AUTHOR.       DOUG NELSON.
        DATE-WRITTEN. MARCH 5, 2002.

      *WRITE LIST STATEMENTS

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER. IBM-PC.
         OBJECT-COMPUTER. IBM-PC.

        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT FNBLIST-FILE
                  ASSIGN TO EXTERNAL FNBLIST
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
        FILE SECTION.

        FD FNBLIST-FILE.
        01 FNBLIST-REC          PIC X(132).

        WORKING-STORAGE SECTION.

        LINKAGE SECTION.
        01 CALL-TYPE               PIC X.
        01 LIST-REC                PIC X(132).

       PROCEDURE DIVISION USING CALL-TYPE, LIST-REC.
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                    :
      * ========== === ================================================:
      * 03/05/2002 DJN WRITE LIST RECORDS                              :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           IF CALL-TYPE = 'O'
             OPEN OUTPUT FNBLIST-FILE
           END-IF
           IF CALL-TYPE = 'W'
             WRITE FNBLIST-REC FROM LIST-REC
           END-IF
           IF CALL-TYPE = 'C'
             CLOSE FNBLIST-FILE
           END-IF
           GOBACK.
