       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDPYX2.
       AUTHOR.        Cowtown.
       DATE-COMPILED.

      *REMARKS. This program appends to PAYADJS_DW
      * every month.

082222******************************************************************
082222*                   C H A N G E   L O G
082222*
082222* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082222*-----------------------------------------------------------------
082222*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082222* EFFECTIVE    NUMBER
082222*-----------------------------------------------------------------
082222* 072018  CR2019????00003  PEMA  New program
082222******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYADJS-IN           ASSIGN TO SYS010.

           SELECT DISK-DATE            ASSIGN TO SYS019.

           SELECT FILE-OUT             ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PAYADJS-IN.

       01  payadjs-rec-in              pic x(80).

       FD  DISK-DATE
                                    COPY ELCDTEFD.

       FD  FILE-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
      
       01  FILE-OUT-REC                PIC X(150).

       WORKING-STORAGE SECTION.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  INPUT-CNT                   PIC 9(11)  VALUE ZEROS.
       77  OUTPUT-CNT                  PIC 9(11)  VALUE ZEROS.

       01  WS-MOE-DATE                 pic x(10).

       01  EXTRACT-RECORD.
           05  EXT-MOE-DATE            PIC X(10).
           05  EXT-CARRIER             PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-RESP-NO             pic x(10).
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-REC-TYPE            PIC X.
           05  EXT-DESC                PIC X(50).
           05  EXT-PMT-TYPE            PIC X.
           05  EXT-AMOUNT-N            pic -9(7).99.
           05  EXT-AMOUNT redefines
               EXT-AMOUNT-N            pic x(11).
           05  EXT-charge-N            pic -9(7).99.
           05  EXT-charge redefines
               EXT-charge-N            pic x(11).
           05  EXT-BILL-FLAG           pic x.

       01  PAYADJS-RECORD.
           12  A-ID                PIC  XXX.
           12  A-SEQ.
               16  A-CARR-GROUP.
                   20  A-CARRIER   PIC  X.
                   20  A-GROUPING  PIC  X(6).
               16  A-REMIT         PIC  X(10).
               16  A-ACCOUNT       PIC  X(10).
           12  A-CO-TYPE           PIC  X.
           12  A-MAINT-DATE        PIC 9(6).
           12  FILLER              PIC  X.
           12  A-DESC              PIC  X(30).
           12  A-TYPE              PIC  X.
               88  A-VALID-TYPE       VALUE 'R' 'D' 'C' 'S' 'T'
                                            'U' 'X' 'Y' 'Z'
                                            'F'.
               88  A-VALID-DMD-TYPE   VALUE 'R' 'D' 'C' 'S' 'T'
                                            'U' 'X' 'Y' 'Z'
                                            'F' 'A' 'B'.
           12  A-AMT               PIC S9(7)V99.
           12  BW-AMT  REDEFINES
               A-AMT               PIC  X(9).
           12  FILLER              PIC  X.
           12  A-BILL-FLAG         PIC  X.
               88  A-BILLED                        VALUE 'B'.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       copy ELCFUNDT.
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           display ' Begin Program CIDPYX2 '

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 (END-OF-INPUT)
      *          OR (CERT-IN-CNT > 100000)

           PERFORM 1050-FINISH-UP      THRU 1050-EXIT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           move spaces to file-out-rec
           string
              'MOE_DATE;'
              'Carrier;' 
              'Grouping;'
              'FinResp;' 
              'Account;' 
              'RecType;' 
              'Description;'
              'PmtType;' 
              'PmtAmt;' 
              'ChgAmt;' 
              'BillFlag;EOR'
              delimited by size into file-out-rec
           end-string
           write file-out-rec
           add 1 to output-cnt

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT PAYADJS-IN output file-out

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' INPUT RECORDS  ' INPUT-CNT
           DISPLAY ' OUTPUT RECORDS ' OUTPUT-CNT
           CLOSE PAYADJS-IN file-out

           .
       0030-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ PAYADJS-IN into PAYADJS-RECORD AT END
               SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1 TO INPUT-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           if a-id = 'CLA'
              continue
           else
              go to 0090-exit
           end-if

           MOVE SPACES                 TO extract-record
           move zeros                  to ext-amount-n
                                          ext-charge-n
           MOVE run-date               TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING
              WS-MM '/'
              WS-DD '/'
              WS-CCYY DELIMITED BY SIZE
              INTO EXT-moe-date
           END-STRING
           MOVE a-CARRIER              TO EXT-CARRIER
           MOVE a-GROUPING             TO EXT-GROUP
           if a-account = low-values
              move spaces              to a-account
           end-if

           MOVE a-ACCOUNT              TO EXT-ACCOUNT
           MOVE a-REMIT                TO EXT-RESP-NO
           if a-co-type = ' ' *> must be a check
              if a-account = spaces or low-values *> must be G
                 move 'G'              to a-co-type
              else
                 move 'A'              to a-co-type
              end-if
           end-if
           move a-co-type              to ext-rec-type *> A OR G
           move A-DESC                 to ext-desc
           move a-type                 to ext-pmt-type
           if a-type = 'R'
              move a-amt               to ext-amount-n
           else
              move a-amt               to ext-charge-n
           end-if
      *    move a-amt                  to ext-amount-n
           move a-bill-flag            to ext-bill-flag

           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           PERFORM 1040-INSERT-ROW     THRU 1040-EXIT

           .
       0100-EXIT.
           EXIT.

       1040-INSERT-ROW.

           move spaces to file-out-rec

           string 
              EXT-MOE-DATE  ';'
              EXT-CARRIER   ';'
              EXT-GROUP     ';'
              EXT-RESP-NO   ';'
              ext-ACCOUNT   ';'
              EXT-REC-TYPE  ';'
              EXT-DESC      ';'
              EXT-PMT-TYPE  ';'
              ext-AMOUNT    ';'
              ext-charge    ';'
              ext-BILL-FLAG ';E'
              delimited by size into file-out-rec
           end-string

           write file-out-rec

           add 1 to output-cnt

           .
       1040-EXIT.
           EXIT.

       1050-finish-up.

           .
       1050-exit.
           exit.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
