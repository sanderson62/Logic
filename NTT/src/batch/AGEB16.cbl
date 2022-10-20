       IDENTIFICATION DIVISION.
       PROGRAM-ID.  AGEB16.
       AUTHOR.       J. CONDON.

      *REMARKS.
      *    THIS SUB-PROGRAM IS CALLED BY BATCH IDEAL PROGRAMS TO SET
      *    UP PARAMETERS NEEDED BY, AND ISSUE CALL TO, THE IBM BAR CODE
      *    ROUTINE (EANSRC).  PARAMETERS PASSED TO THIS PROGRAM ARE

       ENVIRONMENT DIVISION.
      *
      *    This special-names section is necessary and I'd use the set
      *    statement at the top of this file also.
      *
       SPECIAL-NAMES.
      *     CALL-CONVENTION 0 IS callC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       copy "ctypes.cpy".
       01  ws-disp-comp   pic 9(7) value zeros.

      * 01  preloadobj   procedure-pointer.
      * 01  preloaderr   procedure-pointer.
       01  PARM-FLDS.
           05  PARM-BC-TYPE                PIC X(1).
      *
      *        Note that this has been increased from 28 to 50
      *
           05  PARM-BC-IN                  PIC X(50).
           05  PARM-BC-OUT                 PIC X(128).
      *     05  PARM-TEST                   PIC S9(5) COMP-5 VALUE ZERO.
       01  parm-bc-len       pic s9(4) comp-5.
       01  parm-bc-rc        pic s9(4) comp-5.

       LINKAGE SECTION.
       01  PARM-LIST.
          05  BC-LENGTH              PIC s9(4) comp. 
          05  BC-CODE-IN                   PIC X(28).
          05  BC-CODE-OUT                  PIC X(128).


       PROCEDURE DIVISION USING PARM-LIST.

      * set preloadobj to entry "barcode".
      * set preloaderr to entry "doesnotexit".
      * if preloadobj equal preloaderr
      *   display "unable to load barcode"
      *   stop run
      * end-if.
            display ' bc code in ' bc-code-in
            MOVE 'C' TO PARM-BC-TYPE.
             MOVE BC-CODE-IN TO PARM-BC-IN.
      *     MOVE '0011600000000000277700001' TO PARM-BC-IN.
            move bc-length to ws-disp-comp
            display ' disp comp len ' ws-disp-comp
            display ' bc len ' bc-length
             MOVE BC-LENGTH TO PARM-BC-LEN.
            MOVE +28 TO PARM-BC-LEN.
            display ' parm bc len 28 ' parm-bc-len
      *      MOVE 27 to PARM-TEST.
            MOVE 'OUT' TO PARM-BC-OUT.
            DISPLAY 'Calling eansrc '.
      *      SET FUNCPTR TO ENTRY 'bcsubr'.
            CALL "eansrc" USING
                BY REFERENCE PARM-BC-TYPE,
                BY REFERENCE PARM-BC-IN,
               	BY REFERENCE PARM-BC-LEN,
                BY REFERENCE PARM-BC-OUT,
                RETURNING PARM-BC-RC.

            MOVE PARM-BC-LEN TO BC-LENGTH.
            MOVE PARM-BC-OUT TO BC-CODE-OUT.
            DISPLAY 'RC was: ' PARM-BC-RC.
            DISPLAY 'Got ' PARM-BC-LEN ' bytes back'.
            DISPLAY 'Out: "' PARM-BC-OUT '"'.
            GOBACK.
