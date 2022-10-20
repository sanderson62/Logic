       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNX001.

pemuni*REMARKS.
      ******************************************************************
      *                                                                *
      *  DESCRIPTION:  CONVERT ONE INPUT FIELD CONTAINING CITY, STATE  *
      *                AND/OR ZIP INTO 3 OUTPUT FIELDS.                *
      *                                                                *
      *  INPUT:  A 50-CHARACTER FIELD WITH CITY, STATE, ZIP.           *
      *                                                                *
      *  OUTPUT: A 30-CHARACTER FIELD FOR CITY NAME.                   *
      *          A 2-CHARACTER  FIELD FOR STATE NAME.                  *
      *          A 9-CHARACTER  FIELD FOR ZIP CODE.                    *
      *                                                                *
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-CITY-ST-ZIP     PIC X(51).
       01  FILLER REDEFINES WS-CITY-ST-ZIP.
           05  WS-CS OCCURS 51 TIMES INDEXED BY CI PIC X.

       01  WS-STATE           PIC X(2).
       01  FILLER REDEFINES WS-STATE.
           05  WS-ST OCCURS 2  TIMES INDEXED BY SI PIC X.

       01  WS-ZIP             PIC X(9).
       01  FILLER REDEFINES WS-ZIP.
           05  WS-ZP OCCURS 9  TIMES INDEXED BY ZI PIC X.



       LINKAGE SECTION.

       01  FNX001-PARMS.
           05  PARM-CITY-ST-ZIP  PIC X(50).
           05  PARM-CITY         PIC X(30).
           05  PARM-STATE        PIC X(02).
           05  PARM-ZIP          PIC X(09).



      *
       PROCEDURE DIVISION USING FNX001-PARMS.
      *
           MOVE SPACES  TO  PARM-CITY.
           MOVE SPACES  TO  PARM-STATE, WS-STATE.
           MOVE SPACES  TO  PARM-ZIP,   WS-ZIP.
           MOVE PARM-CITY-ST-ZIP TO WS-CITY-ST-ZIP.
           SET ZI, SI TO +1.

           PERFORM 1000-MOVE-ZIP   THRU 1000-EXIT
               VARYING CI FROM +50 BY -1
                 UNTIL CI = +0
                    OR ZI = +0.

           PERFORM 2000-MOVE-STATE THRU 2000-EXIT
               VARYING CI FROM +50 BY -1
                 UNTIL CI = +0
                    OR SI = +0.

           MOVE WS-CITY-ST-ZIP TO PARM-CITY.
           MOVE WS-STATE       TO PARM-STATE.
           MOVE WS-ZIP         TO PARM-ZIP.
           GOBACK.


      *
       1000-MOVE-ZIP.
      *
      *  THIS ROUTINE FINDS A SPACE FOLLOWED BY 9 DIGITS
      *    OR A SPACE FOLLOWED BY 5 DIGITS.
      *
           IF WS-CS (CI) = ' '
               GO TO 1000-EXIT.

           IF (WS-CS (CI - 0) IS NUMERIC) AND
              (WS-CS (CI - 1) IS NUMERIC) AND
              (WS-CS (CI - 2) IS NUMERIC) AND
              (WS-CS (CI - 3) IS NUMERIC) AND
              (WS-CS (CI - 4) IS NUMERIC) AND
              (WS-CS (CI - 5) IS NUMERIC) AND
              (WS-CS (CI - 6) IS NUMERIC) AND
              (WS-CS (CI - 7) IS NUMERIC) AND
              (WS-CS (CI - 8) IS NUMERIC) AND
              (WS-CS (CI - 9)  = ' ')
                  SET ZI TO +9
                  PERFORM 1100-MOVE UNTIL ZI = +0
                  GO TO 1000-EXIT.

           IF (WS-CS (CI - 0) IS NUMERIC) AND
              (WS-CS (CI - 1) IS NUMERIC) AND
              (WS-CS (CI - 2) IS NUMERIC) AND
              (WS-CS (CI - 3) IS NUMERIC) AND
              (WS-CS (CI - 4) IS NUMERIC) AND
              (WS-CS (CI - 5)  = ' ')
                  SET ZI TO +5
                  PERFORM 1100-MOVE UNTIL ZI = +0.

       1000-EXIT.
           EXIT.


      *
       1100-MOVE.
      *
           MOVE WS-CS (CI) TO WS-ZP (ZI).
           MOVE ' '        TO WS-CS (CI).
           SET CI DOWN BY +1.
           SET ZI DOWN BY +1.

       1100-EXIT.
           EXIT.


      *
       2000-MOVE-STATE.
      *
      *  THIS ROUTINE FINDS A SPACE OR A COMMA FOLLOWED BY
      *    TWO ALPHABETIC CHARACTERS.
      *
           IF WS-CS (CI) = ' '
               GO TO 2000-EXIT.

           IF (WS-CS (CI - 0) IS ALPHABETIC) AND
              (WS-CS (CI - 0) NOT = ' ')     AND
              (WS-CS (CI - 1) IS ALPHABETIC) AND
              (WS-CS (CI - 1) NOT = ' ')     AND
              (WS-CS (CI - 2) = ' ' OR ',')
                  SET SI TO +2
                  MOVE WS-CS (CI - 0) TO WS-ST (SI)
                  MOVE ' '            TO WS-CS (CI - 0)
                  SET SI DOWN BY +1
                  MOVE WS-CS (CI - 1) TO WS-ST (SI)
                  MOVE ' '            TO WS-CS (CI - 1)
                  SET SI DOWN BY +1
                  MOVE ' '            TO WS-CS (CI - 2).

           IF WS-CS (CI - 3) = ','
               MOVE ' ' TO WS-CS (CI - 3)
           ELSE
           IF WS-CS (CI - 4) = ','
               MOVE ' ' TO WS-CS (CI - 4).

       2000-EXIT.
           EXIT.

