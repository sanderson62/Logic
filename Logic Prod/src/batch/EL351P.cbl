       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL351.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM CALLS PROGRAM EL350 TO GENERATE CLAIM
      *    LETTERS

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


       DATA DIVISION.
       FILE SECTION.

           EJECT
       WORKING-STORAGE SECTION.

       01  SYSTEM-VARIABLES            PIC  X(3272).

       PROCEDURE DIVISION.

       0000-BEGIN-PROGRAM.

           MOVE SPACES        TO  SYSTEM-VARIABLES
           CALL 'EL350PE' USING SYSTEM-VARIABLES
           GOBACK

           .
       0000-END-PROGRAM.

