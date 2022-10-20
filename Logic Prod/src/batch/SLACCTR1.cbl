       IDENTIFICATION DIVISION.
WAVE   PROGRAM-ID.    SLACCTR1.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SL-NAME-TABLE-IN     ASSIGN TO SYS018
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SL-RPTCD1-IN         ASSIGN TO SYS007
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SL-LIC-IN            ASSIGN TO SYS009
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SL-XREF-IN           ASSIGN TO SYS014.
      *       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SL-AGENTS-IN         ASSIGN TO SYS012
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SL-FILE-IN           ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
              
           SELECT SORT-FILE            ASSIGN TO SORTWK1.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011.
           SELECT COMP-FILE-OUT        ASSIGN TO SYS013.
           SELECT PLAN-FILE-OUT        ASSIGN TO SYS015.
           SELECT SL-XREF-OUT         ASSIGN TO SYS016
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CID-NOTE-FILE-OUT    ASSIGN TO SYS017.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.


       FD  SL-RPTCD1-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  SL-RPTCD1-IN-REC           PIC X(114).

       FD  SL-NAME-TABLE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  SL-NAME-TABLE-REC          PIC X(107).

       FD  SL-LIC-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  SL-LIC-IN-REC              PIC X(125).

       FD  SL-XREF-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SL-XREF-IN-REC             PIC X(20).

       FD  SL-AGENTS-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SL-AGENTS-IN-REC           PIC X(255).

       FD  SL-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SL-INPUT                   PIC X(432).

       SD  SORT-FILE. 

       01  SORT-RECORD.
           05  FILLER                  PIC XX.
           05  SORT-KEY                PIC X(26).
           05  FILLER                  PIC X(1972).

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(2000).

       FD  COMP-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  COMP-RECORD-OUT             PIC X(700).

       FD  PLAN-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  PLAN-RECORD-OUT             PIC X(420).

       FD  SL-XREF-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SL-XREF-OUT-REC            PIC X(20).

       FD  CID-NOTE-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-NOTE-RECORD-OUT         PIC X(120).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SLACCTR1   WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-HOME-OFFICE              PIC X(10)  VALUE ZEROS.
       77  WS-PREV-ST                  PIC XX     VALUE '  '.
       77  WS-PLAN-CNT                 PIC 9(9)   VALUE ZEROS.
       77  WS-CURRENT-BIN-DT           PIC XX     VALUE LOW-VALUES.
       77  WS-WRK-COMM                 PIC SV9(5) COMP-3 VALUE +0.
       77  WS-NO-OF-GAS                PIC 9  VALUE ZEROS.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE               VALUE 'Y'.
       77  WS-GRP-CNT                  PIC 9(6)   VALUE ZEROS.
       77  WS-NGRP-CNT                 PIC 9999   VALUE 0100.
       77  WS-PREV-GRP                 PIC 9(5)   VALUE 99999.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  MR1                         PIC S999   COMP-3 VALUE +73.
       77  P1                          PIC S999   COMP-3 VALUE +0.
       77  M1                          PIC S999   COMP-3 VALUE +0.
       77  R1                          PIC S999   COMP-3 VALUE +0.
       77  A1                          PIC S999   COMP-3 VALUE +0.
       77  S1                          PIC S999   COMP-3 VALUE +0.
       77  S2                          PIC S9(5)  COMP-3 VALUE +0.
       77  S3                          PIC S9(5)  COMP-3 VALUE +0.
       77  S4                          PIC S9(5)  COMP-3 VALUE +0.
       77  T2                          PIC S999   COMP-3 VALUE +0.
       77  WN1                         PIC S999   COMP-3 VALUE +0.
       77  BUSNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-NOTE-OUTPUT-CNT          PIC 9(9)   VALUE ZEROS.
       77  WS-INIT-NOTE                PIC X(120).
       77  WS-INIT-ACCT                PIC X(2000).
       77  WS-SAVE-ACCT                PIC X(2000).
       77  WS-SAVE-COMP                PIC X(700).
       77  WS-SAVE-SL-ACCOUNT          PIC X(10)  VALUE SPACES.
       77  WS-SAVE-SL-AGENT            PIC X(10)  VALUE SPACES.
       77  WS-INIT-COMP                PIC X(700).
       77  WS-RETRO-CNT                PIC 9(7)   VALUE ZEROS.
       77  WS-A-COMP-RECS              PIC 9(7)   VALUE ZEROS.
       77  WS-G-COMP-RECS              PIC 9(7)   VALUE ZEROS.
       77  WS-CO-ENTITY                PIC XX     VALUE SPACES.
       77  WS-CO-FAX-NO                PIC X(10)  VALUE SPACES.
       77  WS-ORIG-UNI-ACCT            PIC X(10).
       77  WS-SAVE-ORIG-UNI-ACCT       PIC X(10).
       77  WS-HOLD-ERPLAN              PIC X(420) VALUE SPACES.
       77  WS-TM-TERR                  PIC XXX    VALUE SPACES.
       77  WS-RM-TERR                  PIC XXX    VALUE SPACES.
       77  WS-COMP-TERR                PIC XXX    VALUE SPACES.
       77  WS-ACNT-SW                  PIC X      VALUE SPACES.
           88  NEED-TO-WRITE-NOTE        VALUE 'Y'.
       01  WS-WORK-NAME1               PIC X(50)  VALUE SPACES.
       01  WS-WORK-NAME2               PIC X(50)  VALUE SPACES.
       01  WS-WORK-ADDRESS             PIC X(40)  VALUE SPACES.
       01  WS-WORK-STREET              PIC X(40)  VALUE SPACES.
       01  WS-WORK-CITY                PIC X(40)  VALUE SPACES.
       01  WS-WORK-STATE               PIC XXX    VALUE SPACES.
       01  WS-WORK-ZIP                 PIC X(10)  VALUE SPACES.
       01  WS-OW.
           05  FILLER                  PIC X(7) VALUE '0001200'.
           05  WS-OW-SEQ               PIC 999  VALUE 200.
       01  WS-WORK-AGENT-NO.
           05  FILLER                  PIC X(5)   VALUE '00013'.
           05  WS-GA-CNTR              PIC 9(5)   VALUE 00100.
       01  WS-WORK-ACCOUNT.
           05  WS-WORK-ACCT-06         PIC XX     VALUE '06'.
           05  WS-WORK-ACCT-SEQ        PIC 9(8)   VALUE 1.
       01  SL-ACCT-RECORD.
           05  SL-ACCOUNT             PIC X(10).
           05  SL-STATE               PIC XX.
           05  SL-EFF-DT              PIC X(8).
           05  SL-EXP-DT              PIC X(8).
           05  SL-NAME                PIC X(45).
           05  SL-PADDR1              PIC X(35).
           05  SL-PADDR2              PIC X(35).
           05  SL-PCITYST             PIC X(35).
           05  SL-PZIP                PIC X(9).
           05  SL-MADDR1              PIC X(35).
           05  SL-MADDR2              PIC X(35).
           05  SL-MCITYST             PIC X(35).
           05  SL-MZIP                PIC X(9).
           05  SL-PHONE               PIC X(10).
           05  SL-TAXID               PIC X(11).
           05  SL-FAX-NO              PIC X(10).
           05  SL-ZCB-IND             PIC X.
           05  SL-AGT-TYPE            PIC X.
           05  SL-SL-COMM             PIC V9(5).
           05  SL-SL-CHAR REDEFINES SL-SL-COMM
                                       PIC X(5).
           05  SL-JL-COMM             PIC V9(5).
           05  SL-JL-CHAR REDEFINES SL-JL-COMM
                                       PIC X(5).
           05  SL-SA-COMM             PIC V9(5).
           05  SL-SA-CHAR REDEFINES SL-SA-COMM
                                       PIC X(5).
           05  SL-JA-COMM             PIC V9(5).
           05  SL-JA-CHAR REDEFINES SL-JA-COMM
                                       PIC X(5).
           05  SL-OA-COMM             PIC V9(5).
           05  SL-OA-CHAR REDEFINES SL-OA-COMM
                                       PIC X(5).
           05  CSO-SL-COMM             PIC V9(5).
           05  CSO-SL-CHAR REDEFINES CSO-SL-COMM
                                       PIC X(5).
           05  CSO-JL-COMM             PIC V9(5).
           05  CSO-JL-CHAR REDEFINES CSO-JL-COMM
                                       PIC X(5).
           05  CSO-SA-COMM             PIC V9(5).
           05  CSO-SA-CHAR REDEFINES CSO-SA-COMM
                                       PIC X(5).
           05  CSO-JA-COMM             PIC V9(5).
           05  CSO-JA-CHAR REDEFINES CSO-JA-COMM
                                       PIC X(5).
           05  CSO-OA-COMM             PIC V9(5).
           05  CSO-OA-CHAR REDEFINES CSO-OA-COMM
                                       PIC X(5).
           05  SL-GRP-ID              PIC XXX.
           05  SL-GRP-NAME            PIC X(40).
           05  SL-REIN-NO             PIC XXXX.
           05  SL-REIN-TYPE           PIC X.

                                       COPY ERCACCT.

                                       COPY ERCCOMP.

                                       COPY ERCPLAN.

                                       COPY ERCACNT.
       01  WS-MISC.
           05  WS-WORK-DATE            PIC X(8).
           05  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                       PIC 9(8).
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.

       01  NT1                         PIC S9(5) COMP-3 VALUE +0.
       01  MNT1                        PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-NAME-TABLE OCCURS 350.
               10  WS-NT-ACCT-NO       PIC X(10).
               10  WS-NT-ACCT-NAME     PIC X(50).
               10  WS-NT-CONTR-NAME    PIC X(47).
           

       01  FILLER.
           05  WS-TERR-TABLE.
               10  FILLER              PIC X(13) VALUE '000BURR      '.
               10  FILLER              PIC X(13) VALUE '000SL        '.
               10  FILLER              PIC X(13) VALUE '003BRUCE     '.
               10  FILLER              PIC X(13) VALUE '004GRISTY    '.
               10  FILLER              PIC X(13) VALUE '005DUFFER    '.
               10  FILLER              PIC X(13) VALUE '007NASSAR    '.
               10  FILLER              PIC X(13) VALUE '008AVERY     '.
               10  FILLER              PIC X(13) VALUE '009GRAZIER   '.
               10  FILLER              PIC X(13) VALUE '010HUDSON    '.
               10  FILLER              PIC X(13) VALUE '011BEAUCHAMP '.
               10  FILLER              PIC X(13) VALUE '014CALLIER   '.
               10  FILLER              PIC X(13) VALUE '015STAYLOR   '.
               10  FILLER              PIC X(13) VALUE '016BURR      '.
               10  FILLER              PIC X(13) VALUE '204PORTER    '.
           05  WS-TERR REDEFINES WS-TERR-TABLE OCCURS 14.
               10  WS-TERR-ID          PIC XXX.
               10  WS-REPORT-CODE-1    PIC X(10).

       01  FILLER.
           05  WS-GRP-ID-TABLE.
               10  FILLER              PIC X(13) VALUE '002ASAMUELS  '.
               10  FILLER              PIC X(13) VALUE '003ARCHER    '.
               10  FILLER              PIC X(13) VALUE '004ANCIRA    '.
               10  FILLER              PIC X(13) VALUE '008JMC AUTO  '.
               10  FILLER              PIC X(13) VALUE '009CRENWELGE '.
               10  FILLER              PIC X(13) VALUE '010ALLWAYS   '.
               10  FILLER              PIC X(13) VALUE '012CASA      '.
               10  FILLER              PIC X(13) VALUE '014CLASSIC   '.
               10  FILLER              PIC X(13) VALUE '016COVERT    '.
               10  FILLER              PIC X(13) VALUE '018DILORENZO '.
               10  FILLER              PIC X(13) VALUE '019DEMONTROND'.
               10  FILLER              PIC X(13) VALUE '021DURANT    '.
               10  FILLER              PIC X(13) VALUE '022GILLMAN   '.
               10  FILLER              PIC X(13) VALUE '023HENNA     '.
               10  FILLER              PIC X(13) VALUE '026HOY FOX   '.
               10  FILLER              PIC X(13) VALUE '027KAHLIG    '.
               10  FILLER              PIC X(13) VALUE '033MMASSEY   '.
               10  FILLER              PIC X(13) VALUE '034MANKINS   '.
               10  FILLER              PIC X(13) VALUE '036MELLOY    '.
               10  FILLER              PIC X(13) VALUE '037MUNDAY    '.
               10  FILLER              PIC X(13) VALUE '038OGDEN     '.
               10  FILLER              PIC X(13) VALUE '040POLLARD   '.
               10  FILLER              PIC X(13) VALUE '042SAMES     '.
               10  FILLER              PIC X(13) VALUE '046TOLIVER   '.
               10  FILLER              PIC X(13) VALUE '047TRANUM    '.
               10  FILLER              PIC X(13) VALUE '050GLOFF     '.
               10  FILLER              PIC X(13) VALUE '053HOUSTONSAT'.
               10  FILLER              PIC X(13) VALUE '055RINGLER   '.
               10  FILLER              PIC X(13) VALUE '056RANDALL   '.
               10  FILLER              PIC X(13) VALUE '058REDRIVER  '.
               10  FILLER              PIC X(13) VALUE '059LUNDGREN  '.
               10  FILLER              PIC X(13) VALUE '060VANBURKLEO'.
               10  FILLER              PIC X(13) VALUE '061MAUND     '.
               10  FILLER              PIC X(13) VALUE '062HARDEMAN  '.
               10  FILLER              PIC X(13) VALUE '067DDH       '.
               10  FILLER              PIC X(13) VALUE '073MANUELAUTO'.
               10  FILLER              PIC X(13) VALUE '074SWHYUNDAI '.
               10  FILLER              PIC X(13) VALUE '075ODIAMONDS '.
               10  FILLER              PIC X(13) VALUE '077KINSEL    '.
               10  FILLER              PIC X(13) VALUE '078MPERRY    '.
               10  FILLER              PIC X(13) VALUE '079GARCIA    '.
               10  FILLER              PIC X(13) VALUE '080LJOHNSON  '.
               10  FILLER              PIC X(13) VALUE '082STERLING  '.
               10  FILLER              PIC X(13) VALUE '083CRAWFORD  '.
               10  FILLER              PIC X(13) VALUE '084CLARK     '.
               10  FILLER              PIC X(13) VALUE '086FKENTAUTO '.
               10  FILLER              PIC X(13) VALUE '087CLSICTYLER'.
               10  FILLER              PIC X(13) VALUE '088BACON     '.
               10  FILLER              PIC X(13) VALUE '089GJORDAN   '.
               10  FILLER              PIC X(13) VALUE '090RODRIGUEZ '.
               10  FILLER              PIC X(13) VALUE '091BOBBY FORD'.
               10  FILLER              PIC X(13) VALUE '093SISK      '.
               10  FILLER              PIC X(13) VALUE '094FTWORTHSAT'.
               10  FILLER              PIC X(13) VALUE '095RICHARDSON'.
               10  FILLER              PIC X(13) VALUE '096GULF COAST'.
               10  FILLER              PIC X(13) VALUE '098WORLD CAR '.
               10  FILLER              PIC X(13) VALUE '099CROWN     '.
               10  FILLER              PIC X(13) VALUE '100AUTO GRP  '.
               10  FILLER              PIC X(13) VALUE '101TWIN CITY '.
               10  FILLER              PIC X(13) VALUE '102DOWAUTOGRP'.
               10  FILLER              PIC X(13) VALUE '103RUDOLPH   '.
               10  FILLER              PIC X(13) VALUE '105PAUL YOUNG'.
               10  FILLER              PIC X(13) VALUE '158GOODARD   '.
               10  FILLER              PIC X(13) VALUE '173BECKMASTEN'.
               10  FILLER              PIC X(13) VALUE '174MUELLER   '.
               10  FILLER              PIC X(13) VALUE '179MERCER    '.
               10  FILLER              PIC X(13) VALUE '185ACCESSAUTO'.
               10  FILLER              PIC X(13) VALUE '187VARELA    '.
               10  FILLER              PIC X(13) VALUE '188BARNETT   '.
               10  FILLER              PIC X(13) VALUE '200TEAMBONNER'.
               10  FILLER              PIC X(13) VALUE '201YAKLIN    '.
               10  FILLER              PIC X(13) VALUE '202KLEMENT   '.
               10  FILLER              PIC X(13) VALUE '203HACIENDA  '.
           05  WS-GROUP-IDS REDEFINES WS-GRP-ID-TABLE OCCURS 73.
               10  WS-GROUP-ID         PIC XXX.
               10  WS-REPORT-CODE-2    PIC X(10).


       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.
           

       01  T1                          PIC S999   COMP-3 VALUE +1.
       01  WS-TABLE-1.
           05  WS-SEQ-NO-TABLE OCCURS 70.
               10  WS-TBL-ACCT-ST      PIC XX.
               10  WS-TBL-ACCT-SEQ     PIC 9(6).


       01  UM1                         PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-UNI-RPTCD1-TABLE OCCURS 70.
               10  WS-UNI-STATE        PIC XX.
               10  FILLER              PIC X.
               10  WS-UNI-REPORT-CD-1  PIC X(10).
               10  WS-UNI-HO-AGT       PIC X(10).
               10  WS-UNI-COMMENT-1    PIC X(47).
               10  WS-UNI-COMMENT-2    PIC X(43).
               10  FILLER              PIC X.

       01  UM2                         PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-UNI-XREF-TABLE OCCURS 1000.
               10  WS-UNI-ACCOUNT      PIC X(10).
               10  WS-LOGIC-ACCOUNT    PIC X(10).

       01  SL1                         PIC S9(5) COMP-3 VALUE +0.
       01  SM1                         PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-SL-AGT-NOS-TABLE OCCURS 100.
               10  WS-SL-AGT-NAME      PIC X(45).
               10  WS-SL-AGT-NO        PIC X(10).

       01  UM3                         PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-SL-AGENT-TABLE OCCURS 500.
               10  WS-AGT-ACCOUNT      PIC X(10).
               10  WS-AGT-TERR         PIC XXX.
               10  WS-AGENT-NAME       PIC X(45).
               10  WS-AGENT-ADDR1      PIC X(35).
               10  WS-AGENT-ADDR2      PIC X(35).
               10  WS-AGENT-ADDR3      PIC X(35).
               10  WS-AGENT-ZIP        PIC X(9).
               10  WS-AGENT-PHONE      PIC X(10).
               10  WS-AGENT-TAX-ID     PIC X(11).
               10  WS-AGENT-FAX-NO     PIC X(10).
               10  WS-AGENT-TYPE       PIC XX.
               10  WS-AGENT-SL-COMM    PIC V9(5).
               10  WS-AGENT-SL-CHAR REDEFINES
                 WS-AGENT-SL-COMM      PIC X(5).
               10  WS-AGENT-JL-COMM    PIC V9(5).
               10  WS-AGENT-JL-CHAR REDEFINES
                 WS-AGENT-JL-COMM      PIC X(5).
               10  WS-AGENT-SA-COMM    PIC V9(5).
               10  WS-AGENT-SA-CHAR REDEFINES
                 WS-AGENT-SA-COMM      PIC X(5).
               10  WS-AGENT-JA-COMM    PIC V9(5).
               10  WS-AGENT-JA-CHAR REDEFINES
                 WS-AGENT-JA-COMM      PIC X(5).
               10  WS-AGENT-OA-COMM    PIC V9(5).
               10  WS-AGENT-OA-CHAR REDEFINES
                 WS-AGENT-OA-COMM      PIC X(5).
               10  WS-AGENT-SL-CSO     PIC V9(5).
               10  WS-AGENT-JL-CSO     PIC V9(5).
               10  WS-AGENT-SA-CSO     PIC V9(5).
               10  WS-AGENT-JA-CSO     PIC V9(5).
               10  WS-AGENT-OA-CSO     PIC V9(5).
               10  WS-AGENT-NUMBER     PIC X(10).

       01  UM4                         PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-UNI-LIC-TABLE OCCURS 500.
               10  WS-LIC-ACCOUNT      PIC X(10).
               10  WS-LIC-DLR-LIC-NO   PIC X(15).
               10  WS-LIC-NAME1        PIC X(50).
               10  WS-LIC-NAME2        PIC X(50).

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           SORT SORT-FILE ON ASCENDING KEY SORT-KEY
                INPUT PROCEDURE 0002-INPUT
                                       THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT
                                       THRU 0003-EXIT

           GOBACK
             .

       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0040-READ-TABLES    THRU 0040-EXIT

           PERFORM 0045-ASSIGN-GA-NUMBERS
                                       THRU 0045-EXIT

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT
      *       OR WS-INPUT-CNT > 200

           RELEASE SORT-RECORD   FROM ACCOUNT-MASTER



      *    PERFORM 0095-BUILD-COMP     THRU 0095-EXIT
      *    RELEASE SORT-RECORD         FROM ACCOUNT-MASTER

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           MOVE ' '                    TO WS-EOF-SW

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           PERFORM 0071-PROCESS-RECS THRU 0071-EXIT UNTIL
                END-OF-INPUT

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 = UM2
              WRITE SL-XREF-OUT-REC   FROM WS-UNI-XREF-TABLE (S1)
           END-PERFORM
           MOVE '9999999999'           TO SL-XREF-OUT-REC (1:10)
           MOVE WS-WORK-AGENT-NO       TO SL-XREF-OUT-REC (11:10)
           WRITE SL-XREF-OUT-REC
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.


       0010-INITIALIZE.

           ACCEPT DC-GREG-DATE-1-YMD   FROM DATE
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
           ELSE
              DISPLAY ' CURRENT DATE ERROR     '
           END-IF

           MOVE ZEROS                  TO WS-TABLE-1
           MOVE SPACES                 TO ACCOUNT-MASTER
           MOVE 'AM'                   TO AM-RECORD-ID
           MOVE X'04'                  TO AM-COMPANY-CD
                                          AM-COMPANY-CD-A1
           MOVE '9'                    TO AM-CARRIER
                                          AM-VG-CARRIER
           MOVE '000000'               TO AM-GROUPING
                                          AM-VG-GROUPING
           MOVE 'TX'                   TO AM-STATE
                                          AM-VG-STATE
           MOVE 'CONV'                 TO AM-LAST-MAINT-USER
           MOVE +200000                TO AM-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO AM-LAST-MAINT-DT
           MOVE ZEROS                  TO AM-TEL-NO
                                          AM-ANNIVERSARY-DATE
                                          AM-HI-CERT-DATE
                                          AM-LO-CERT-DATE
                                          AM-ENTRY-DATE
                                          AM-INACTIVE-DATE
                                          AM-EFFECT-DT
                                          AM-CAL-TABLE
                                          AM-LF-DEVIATION
                                          AM-AH-DEVIATION
           MOVE 02                     TO AM-3RD-PARTY-NOTIF-LEVEL

           MOVE 01                     TO AM-GPCD
           MOVE 01                     TO AM-REMIT-TO
           MOVE '2'                    TO AM-IG
           MOVE +0                     TO AM-LF-DEVIATION-PCT
                                          AM-AH-DEVIATION-PCT
                                          AM-CLP-TOL-PCT
                                          AM-SPP-LEASE-COMM
                                          AM-LF-OB-RATE
                                          AM-AH-OB-RATE
                                          AM-LF-OB-RATE-JNT
                                          AM-AH-OB-RATE-JNT
                                          AM-LF-PSI-FACTOR
                                          AM-AH-PSI-FACTOR
                                          AM-OVR-SHT-AMT
                                          AM-OVR-SHT-PCT
                                          AM-REI-FEE-LF
                                          AM-REI-FEE-AH
                                          AM-REI-LF-TAX
                                          AM-REI-PR-PCT
                                          AM-REI-78-PCT
                                          AM-REI-AH-TAX
                                          AM-TOL-PREM
                                          AM-TOL-REF
                                          AM-TOL-CLM
                                          AM-LF-RET
                                          AM-AH-RET
                                          AM-RET-MIN-LOSS-L
                                          AM-RET-MIN-LOSS-A
                                          AM-LF-RPT021-EXP-PCT
                                          AM-AH-RPT021-EXP-PCT
                                          AM-MAX-MON-BEN
                                          AM-MAX-TOT-BEN
                                          AM-CANCEL-FEE 
                                          AM-TOL-REF-PCT
                                          AM-TARGET-LOSS-RATIO    
                                          AM-LIFE-IBNR-PCT        
                                          AM-CRDT-MODIFICATION-PCT
                                          AM-EXEC1-DIS-PERCENT  
                                          AM-EXEC1-LIFE-PERCENT 
                                          AM-EXEC2-DIS-PERCENT  
                                          AM-EXEC2-LIFE-PERCENT 
                                          AM-RETRO-QUALIFY-LIMIT
                                          AM-RETRO-RET-PCT-LF (1) 
                                          AM-RETRO-RET-THRU-LF (1)
                                          AM-RETRO-RET-PCT-LF (2) 
                                          AM-RETRO-RET-THRU-LF (2)
                                          AM-RETRO-RET-PCT-LF (3) 
                                          AM-RETRO-RET-THRU-LF (3)

           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE +0                  TO AM-L-COM (SUB1)
                                          AM-J-COM (SUB1)
                                          AM-A-COM (SUB1)
                                          AM-L-COM-SV (SUB1)
                                          AM-J-COM-SV (SUB1)
                                          AM-A-COM-SV (SUB1)
              MOVE ZEROS               TO AM-AGT (SUB1)
              IF SUB1 < +6
                 MOVE +0               TO AM-ALLOW-BEGIN-RANGE (SUB1)
                                          AM-ALLOW-END-RANGE (SUB1)
                                          AM-ALLOWANCE-AMT (SUB1)
              END-IF
           END-PERFORM

           MOVE 'Y'                    TO AM-BENEFIT-TABLE-USAGE
                                          AM-BENEFIT-RETRO-Y-N (1)
                                          AM-BENEFIT-RETRO-Y-N (2)
           MOVE '99'                   TO AM-BENEFIT-CODE (1)
                                          AM-BENEFIT-CODE (2)
           MOVE 'L'                    TO AM-BENEFIT-TYPE (1)
           MOVE 'A'                    TO AM-BENEFIT-TYPE (2)
           MOVE '100'                  TO AM-BENEFIT-REVISION (1)
                                          AM-BENEFIT-REVISION (2)
           MOVE ACCOUNT-MASTER         TO WS-INIT-ACCT

           MOVE SPACES                 TO COMPENSATION-MASTER
           INITIALIZE                     COMPENSATION-MASTER
           MOVE 'CO'                   TO CO-RECORD-ID
           MOVE X'04'                  TO CO-COMPANY-CD
           MOVE 'CONV'                 TO CO-LAST-MAINT-USER
           MOVE +200000                TO CO-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO CO-LAST-MAINT-DT
           MOVE LOW-VALUES             TO CO-ROLADEX-PRINT-DT
                                          CO-LAST-EOM-STMT-DT
                                          CO-GA-EFFECTIVE-DT
                                          CO-GA-TERMINATION-DT
           MOVE ZEROS                  TO CO-LAST-ACTIVITY-DATE
                                          CO-LAST-STMT-DT
                                          CO-CURRENT-LAST-STMT-DT
                                          CO-FAXNO
                                          CO-TELEPHONE
           MOVE 'B'                    TO CO-BILL-SW
           MOVE COMPENSATION-MASTER    TO WS-INIT-COMP

           MOVE SPACES                 TO PLAN-MASTER
           MOVE 'PL'                   TO PL-RECORD-ID
           MOVE ZEROS                  TO PL-TOL-PREM-AMT
                                          PL-TOL-REF-AMT
                                          PL-TOL-CLM-AMT
                                          PL-TOL-PREM-PCT
                                          PL-TOL-REF-PCT
                                          PL-TOL-CLM-PCT
                                          PL-OVER-SHORT-AMT
                                          PL-OVER-SHORT-PCT
                                          PL-POLICY-FEE
                                          PL-STATE-TAX
                                          PL-RETRO-RET
                                          PL-DEV-PCT
                                          PL-SALES-TAX
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > 8
              MOVE ZEROS               TO PL-LM-AGE (S1)
                                          PL-LM-DUR (S1)
                                          PL-LM-MOA (S1)
                                          PL-LM-AMT (S1)
           END-PERFORM

           MOVE 71                     TO PL-ATT-AGE
           MOVE 70                     TO PL-LM-AGE (1)
           MOVE 120                    TO PL-LM-DUR (1)
           MOVE 1                      TO PL-LM-MOA (1)
                                          PL-LM-AMT (1)
           MOVE WS-CURRENT-BIN-DT      TO PL-LAST-MAINT-DT
           MOVE +230000                TO PL-LAST-MAINT-HHMMSS
           MOVE 'CONV'                 TO PL-LAST-MAINT-USER

           MOVE PLAN-MASTER            TO WS-HOLD-ERPLAN

           MOVE SPACES                 TO NOTE-FILE
           MOVE 'NT'                   TO NT-FILE-ID
           MOVE X'04'                  TO NT-COMPANY-CD
           MOVE '3'                    TO NT-RECORD-TYPE
           MOVE +1                     TO NT-LINE-SEQUENCE
           MOVE 'CONV'                 TO NT-LAST-MAINT-BY
           MOVE +200000                TO NT-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO NT-LAST-MAINT-DT
           MOVE 'RECORD CREATED BY SAL CONVERSION 06/30/03'
                                       TO NT-NOTE-LINE
           MOVE NOTE-FILE              TO WS-INIT-NOTE

           PERFORM VARYING SL1 FROM +1 BY +1 UNTIL
              SL1 > +100
              MOVE SPACES              TO WS-SL-AGT-NAME (SL1)
                                          WS-SL-AGT-NO   (SL1)
           END-PERFORM

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SL-FILE-IN
                      SL-NAME-TABLE-IN
                      SL-RPTCD1-IN
                      SL-LIC-IN
                      SL-XREF-IN
                      SL-AGENTS-IN
      *               SL-TABLE-IN
      *               SL-TABLE-IN-GRP
               OUTPUT CID-FILE-OUT
                      COMP-FILE-OUT
                      PLAN-FILE-OUT
                      SL-XREF-OUT
                      CID-NOTE-FILE-OUT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY '  SL  RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' ACCT RECORDS OUTPUT   ' WS-OUTPUT-CNT
           DISPLAY ' PLAN RECORDS OUTPUT   ' WS-PLAN-CNT
           DISPLAY ' ACCOUNTS WITH RETROS  ' WS-RETRO-CNT
           DISPLAY ' G ERCOMP RECS         ' WS-G-COMP-RECS
           DISPLAY ' A ERCOMP RECS         ' WS-A-COMP-RECS
           DISPLAY ' NOTE RECORDS OUT      ' WS-NOTE-OUTPUT-CNT

           CLOSE SL-FILE-IN
                 SL-NAME-TABLE-IN
                 SL-XREF-IN
                 CID-FILE-OUT
                 SL-RPTCD1-IN
                 SL-LIC-IN
                 COMP-FILE-OUT
                 PLAN-FILE-OUT
                 SL-AGENTS-IN
                 SL-XREF-OUT
      *          SL-TABLE-IN
      *          SL-TABLE-IN-GRP
                 CID-NOTE-FILE-OUT


           .
       0030-EXIT.
           EXIT.

       0040-READ-TABLES.

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ SL-RPTCD1-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE SL-RPTCD1-IN-REC
                                       TO WS-UNI-RPTCD1-TABLE (S1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM S1
           MOVE S1                     TO UM1
           DISPLAY ' NUMBER OF REPORT CODE 1 RECORDS ' S1

           MOVE ' '                    TO WS-TABLE-EOF-SW

           MOVE +1                     TO T1
           PERFORM VARYING S2 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ SL-XREF-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 IF SL-XREF-IN-REC (1:10) = '9999999999'
                    MOVE SL-XREF-IN-REC (11:10)
                                       TO WS-WORK-AGENT-NO
                 ELSE
                    MOVE SL-XREF-IN-REC
                                       TO WS-UNI-XREF-TABLE (S2)
                 END-IF
                 IF S2 = +1
                    MOVE WS-UNI-ACCOUNT (S2) (3:2)
                                       TO WS-PREV-ST
                 END-IF
                 IF WS-UNI-ACCOUNT (S2) (3:2) NOT = WS-PREV-ST
                    MOVE WS-UNI-ACCOUNT (S2) (3:2)
                                       TO WS-PREV-ST
                    MOVE WS-UNI-ACCOUNT (S2 - 1) (3:2)
                                       TO WS-TBL-ACCT-ST (T1)
                    MOVE WS-LOGIC-ACCOUNT (S2 - 1) (5:6)
                                       TO WS-TBL-ACCT-SEQ (T1)
                    ADD +1             TO T1
                 END-IF
              END-IF
           END-PERFORM
           MOVE +1                     TO T1

           SUBTRACT +2                 FROM S2
           MOVE S2                     TO UM2
      *                                   WS-WORK-ACCT-SEQ
           DISPLAY ' NUMBER OF XREF TABLE RECORDS ' S2
           MOVE +1                     TO S2
           MOVE ' '                    TO WS-TABLE-EOF-SW

           PERFORM VARYING S3 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ SL-AGENTS-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE SL-AGENTS-IN-REC
                                       TO WS-SL-AGENT-TABLE (S3)
      *          ADD 1                 TO WS-GA-CNTR
      *          MOVE WS-WORK-AGENT-NO TO WS-SL-AGENT-NO (S3)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM S3
           MOVE S3                     TO UM3

           DISPLAY ' NUMBER OF AGENT TABLE RECORDS ' S3
           MOVE +1                     TO S3

           MOVE ' '                    TO WS-TABLE-EOF-SW

           PERFORM VARYING S3 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ SL-LIC-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE SL-LIC-IN-REC   TO WS-UNI-LIC-TABLE (S3)
      *          DISPLAY ' LIC ACCOUNT ' WS-LIC-ACCOUNT (S3)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM S3
           MOVE S3                     TO UM4

           DISPLAY ' NUMBER OF LICENSE     RECORDS ' S3
           MOVE +1                     TO S3

           MOVE ' '                    TO WS-TABLE-EOF-SW

           PERFORM VARYING NT1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ SL-NAME-TABLE-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE SL-NAME-TABLE-REC   TO WS-NAME-TABLE (NT1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM NT1
           MOVE NT1                    TO MNT1

           DISPLAY ' NUMBER OF NAME TABLE  RECORDS ' MNT1
           MOVE +1                     TO NT1

           MOVE ' '                    TO WS-TABLE-EOF-SW

           .
       0040-EXIT.
           EXIT.

       0045-ASSIGN-GA-NUMBERS.

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > UM3
              PERFORM VARYING SL1 FROM +1 BY +1 UNTIL
                 (SL1 > 100)
                 OR (WS-SL-AGT-NAME (SL1) = SPACES)
                 OR (WS-SL-AGT-NAME (SL1) = WS-AGENT-NAME (S1))
              END-PERFORM
              IF SL1 > 100
                 DISPLAY ' AGENT NO TABLE BLOWN '
                 PERFORM ABEND-PGM
              ELSE
                 IF WS-SL-AGT-NAME (SL1) = SPACES
                    MOVE WS-AGENT-NAME (S1)
                                       TO WS-SL-AGT-NAME (SL1)
                    ADD 1              TO WS-GA-CNTR
                    MOVE WS-WORK-AGENT-NO
                                       TO WS-SL-AGT-NO (SL1)
                    IF SL1 > SM1
                       MOVE SL1        TO SM1
                    END-IF
                 END-IF
              END-IF
           END-PERFORM

           DISPLAY ' NUMBER OF AGENT ASSIGN RECORDS ' SM1

           PERFORM VARYING S3 FROM +1 BY +1 UNTIL
              S3 > UM3
              PERFORM VARYING SL1 FROM +1 BY +1 UNTIL
                 (SL1 > SM1)
                 OR (WS-AGENT-NAME (S3) = WS-SL-AGT-NAME (SL1))
              END-PERFORM
              IF SL1 > SM1
                 DISPLAY ' SOMETHING WRONG SOMEWHERE '
                 PERFORM ABEND-PGM
              ELSE
                 MOVE WS-SL-AGT-NO (SL1) TO WS-AGENT-NUMBER (S3)
              END-IF
           END-PERFORM

           .
       0045-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF SL-ACCOUNT NOT = WS-SAVE-SL-ACCOUNT
              IF WS-SAVE-SL-ACCOUNT NOT = SPACES
                 RELEASE SORT-RECORD   FROM ACCOUNT-MASTER
              END-IF
              MOVE SL-ACCOUNT          TO WS-SAVE-SL-ACCOUNT
              PERFORM 0080-PROCESS-INPUT
                                       THRU 0080-EXIT
           ELSE
              IF SL-AGT-TYPE = 'O'
                 PERFORM 0055-ADD-LAST-LEVEL
                                       THRU 0055-EXIT
                 MOVE 'O'              TO AM-COMMENT-LINE (4) (22:1)
              ELSE
                 DISPLAY ' WE HAVE A DUPLICATE ' SL-ACCOUNT
              END-IF
           END-IF

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0055-ADD-LAST-LEVEL.

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (A1 > +10)
              OR (AM-AGT (A1) = ZEROS)
           END-PERFORM
           IF A1 < +11
              ADD 1                    TO WS-OW-SEQ
              MOVE WS-OW               TO AM-AGT (A1)
              MOVE 'P'                 TO AM-COM-TYP (A1)
              PERFORM 0100-WRITE-G-ERCOMP
                                       THRU 0100-EXIT
           ELSE
              DISPLAY ' NO ROOM FOR LAST LEVEL ' SL-ACCOUNT
           END-IF

           MOVE WS-INIT-COMP           TO COMPENSATION-MASTER
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING
           MOVE LOW-VALUES             TO CO-ACCOUNT
           MOVE AM-AGT (A1)            TO CO-RESP-NO
           MOVE 'G'                    TO CO-TYPE


           MOVE AM-NAME                TO CO-ACCT-NAME 
           MOVE AM-ADDRS               TO CO-ADDR-1 
           MOVE AM-CITY                TO CO-ADDR-3 
           MOVE AM-ZIP                 TO CO-ZIP 
           MOVE AM-CSR-CODE            TO CO-CSR-CODE 
           MOVE AM-TEL-NO              TO CO-TELEPHONE 
           MOVE AM-ID-NO               TO CO-SOC-SEC 

           INSPECT CO-ADDR-3
              REPLACING ALL ',' BY ' '

           MOVE SL-FAX-NO              TO CO-FAXNO
           MOVE 'Y'                    TO CO-BALANCE-CONTROL
           MOVE 'N'                 TO CO-CSO-1099

           MOVE COMPENSATION-MASTER    TO WS-SAVE-COMP
           WRITE COMP-RECORD-OUT       FROM COMPENSATION-MASTER
           ADD +1 TO WS-G-COMP-RECS

           .
       0055-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ SL-FILE-IN INTO SL-ACCT-RECORD AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0062-ASSIGN-ACCT-NO.

           ADD 1                       TO WS-WORK-ACCT-SEQ

           MOVE WS-WORK-ACCOUNT        TO AM-ACCOUNT
           MOVE '060000'               TO AM-ACCOUNT (1:6)
           MOVE SL-ACCOUNT (7:4)       TO AM-ACCOUNT (7:4)

      *    PERFORM VARYING S2 FROM +1 BY +1 UNTIL
      *       S2 > UM2
      *       IF SL-ACCOUNT = WS-UNI-ACCOUNT (S2)
      *          DISPLAY ' PAUL WE HAVE A DUP ACCOUNT '
      *             SL-ACCOUNT
      *          MOVE WS-LOGIC-ACCOUNT (S2)
      *                                TO AM-ACCOUNT
      *          MOVE UM2              TO S2
      *       END-IF
      *    END-PERFORM
      *
      *    IF AM-ACCOUNT = SPACES
      *       MOVE '06'                TO WS-WORK-ACCT-08
      *       MOVE SL-ACCOUNT (3:2)   TO WS-WORK-ACCT-ST
      *       PERFORM 0065-SET-NEW-CNT THRU 0065-EXIT
      *       MOVE WS-GRP-CNT          TO WS-WORK-ACCT-SEQ
      *       MOVE WS-WORK-ACCOUNT     TO AM-ACCOUNT
      *       MOVE SL-ACCOUNT         TO WS-UNI-ACCOUNT (UM2)
      *       MOVE AM-ACCOUNT          TO WS-LOGIC-ACCOUNT (UM2)
      *       ADD +1                   TO UM2
      *    END-IF

           DISPLAY ' ' AM-ACCOUNT ' ' SL-ACCOUNT

           .
       0062-EXIT.
           EXIT.

       0065-SET-NEW-CNT.

      *    IF WS-WORK-ACCT-ST = WS-TBL-ACCT-ST (T1)
      *       CONTINUE
      *    ELSE
      *       PERFORM VARYING T1 FROM +1 BY +1 UNTIL
      *          (T1 > +70)
      *          OR (WS-WORK-ACCT-ST = WS-TBL-ACCT-ST (T1))
      *          OR (WS-TBL-ACCT-ST (T1) = ZEROS)
      *       END-PERFORM
      *
      *       EVALUATE TRUE
      *          WHEN T1 > +70
      *             DISPLAY ' STATE ACCOUNT SEQ TABLE BLEW '
      *                SL-ACCOUNT ' ' T1
      *             PERFORM ABEND-PGM
      *          WHEN WS-TBL-ACCT-ST (T1) = ZEROS
      *             MOVE WS-WORK-ACCT-ST
      *                                TO WS-TBL-ACCT-ST (T1)
      *             MOVE ZEROS         TO WS-TBL-ACCT-SEQ (T1)
      *       END-EVALUATE
      *    END-IF
      *
      *    ADD 1                       TO WS-TBL-ACCT-SEQ (T1)
      *    MOVE WS-TBL-ACCT-SEQ (T1)   TO WS-GRP-CNT           

           .
       0065-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-FILE     AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           .
       0070-EXIT.
           EXIT.

       0071-PROCESS-RECS.

           MOVE SORT-RECORD            TO CID-RECORD-OUT
                                          ACCOUNT-MASTER
      *    PERFORM 0077-BUILD-ERNOTE   THRU 0077-EXIT
           WRITE CID-RECORD-OUT
           ADD 1                       TO WS-OUTPUT-CNT

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0071-EXIT.
           EXIT.

       0077-BUILD-ERNOTE.
       
           MOVE WS-INIT-NOTE           TO NOTE-FILE
           
           MOVE AM-CONTROL-A           TO NT-ACCT-NOTE-KEY
           
           IF NT1 > MNT1
              MOVE SL-NAME             TO NT-SHIPPING-LINE
           ELSE
              MOVE WS-NT-ACCT-NAME (NT1)
                                       TO NT-SHIPPING-LINE
           END-IF

           WRITE CID-NOTE-RECORD-OUT   FROM NOTE-FILE
           ADD 1                       TO WS-NOTE-OUTPUT-CNT
           
           .
       0077-EXIT.
           EXIT.

       0078-BUILD-ERPLAN.

           MOVE WS-HOLD-ERPLAN         TO PLAN-MASTER
           MOVE AM-CONTROL-PRIMARY     TO PL-CONTROL-PRIMARY
           MOVE 'L'                    TO PL-BENEFIT-TYPE
           MOVE '99'                   TO PL-BENEFIT-CODE
           MOVE '100'                  TO PL-REVISION-NO

           EVALUATE AM-STATE
              WHEN 'AZ'
                 MOVE 180              TO PL-LM-DUR (1)
              WHEN 'CO'
                 MOVE 65               TO PL-LM-AGE (1)
           END-EVALUATE

           MOVE PLAN-MASTER            TO PLAN-RECORD-OUT
           WRITE PLAN-RECORD-OUT
           ADD 1                       TO WS-PLAN-CNT

           MOVE WS-HOLD-ERPLAN         TO PLAN-MASTER
           MOVE AM-CONTROL-PRIMARY     TO PL-CONTROL-PRIMARY
           MOVE 'A'                    TO PL-BENEFIT-TYPE
           MOVE '99'                   TO PL-BENEFIT-CODE
           MOVE '100'                  TO PL-REVISION-NO

           EVALUATE AM-STATE
              WHEN 'AZ'
                 MOVE 180              TO PL-LM-DUR (1)
              WHEN 'CO'
                 MOVE 65               TO PL-LM-AGE (1)
           END-EVALUATE

           MOVE PLAN-MASTER            TO PLAN-RECORD-OUT
           WRITE PLAN-RECORD-OUT
           ADD 1                       TO WS-PLAN-CNT

           .
       0078-EXIT.
           EXIT.

       0080-PROCESS-INPUT.

           MOVE WS-INIT-ACCT           TO ACCOUNT-MASTER
           MOVE SPACES                 TO WS-WORK-STREET
                                          WS-WORK-CITY
                                          WS-WORK-STATE
                                          WS-WORK-ZIP
           
           PERFORM 0062-ASSIGN-ACCT-NO THRU 0062-EXIT
           MOVE AM-ACCOUNT             TO AM-VG-ACCOUNT
                                          AM-USER-FIELDS
           MOVE SL-STATE              TO AM-STATE
                                          AM-VG-STATE

           MOVE SL-ACCOUNT            TO AM-ORIG-DEALER-NO
      *    IF AM-STATE = WS-UNI-STATE (S1)
      *       CONTINUE
      *    ELSE
      *       PERFORM VARYING S1 FROM +1 BY +1 UNTIL
      *          (AM-STATE = WS-UNI-STATE (S1))
      *          OR (S1 > UM1)
      *       END-PERFORM
      *    END-IF
      *    IF S1 > UM1
      *       MOVE SPACES              TO WS-HOME-OFFICE
      *       MOVE +1                  TO S1
      *       DISPLAY ' REPORT CODE 1 TABLE NOT FND '
      *          AM-ACCOUNT ' ' AM-STATE
      *       PERFORM ABEND-PGM
      *    ELSE
      *       MOVE WS-UNI-HO-AGT (S1)  TO WS-HOME-OFFICE
      *       MOVE WS-UNI-REPORT-CD-1 (S1)
      *                                TO AM-REPORT-CODE-1
      *       MOVE WS-UNI-COMMENT-1 (S1)
      *                                TO AM-COMMENT-LINE (1)
      *       MOVE WS-UNI-COMMENT-2 (S1)
      *                                TO AM-COMMENT-LINE (2)
      *    END-IF

           EVALUATE AM-STATE
              WHEN 'AZ'
                 MOVE 'ALWA'           TO AM-CSR-CODE
                 MOVE '04'             TO AM-CAL-TABLE
                 MOVE 'AZ $1-BA2-NO HA 11-1-07'
                                       TO AM-COMMENT-LINE (1)
                 MOVE '30470C AZ (CID-529)'
                                       TO AM-COMMENT-LINE (2)
              WHEN 'CO'
                 MOVE 'SPJA'           TO AM-CSR-CODE
                 MOVE 'CR'             TO AM-CAL-TABLE
                 MOVE 'CO $1-BA1-NO HA 11-17-08 30480C CO 2nd Rev'
                                       TO AM-COMMENT-LINE (1)
                 MOVE '30480C CO 2nd Rev (CID-531)'
                                       TO AM-COMMENT-LINE (2)
              WHEN 'NM'
                 MOVE 'SPJA'           TO AM-CSR-CODE
                 MOVE 'NM $1-BA2-NO HA 11-1-07'
                                       TO AM-COMMENT-LINE (1)
                 MOVE '30470C NM (25) (CID-468)'
                                       TO AM-COMMENT-LINE (2)
              WHEN 'SC'
                 MOVE 'AKNA'           TO AM-CSR-CODE
                 MOVE 'SC $1-BA2-NO HA-30491C SC 3-27-09'
                                       TO AM-COMMENT-LINE (1)
                 MOVE '30491C SC (CID-522)'
                                       TO AM-COMMENT-LINE (2)
              WHEN 'TX'
                 MOVE 'AKNA'           TO AM-CSR-CODE
                 MOVE '1'              TO AM-IG
                 MOVE 'EE'             TO AM-CAL-TABLE
                 MOVE 'TX $1-BA2-NO HA 11-1-07'
                                       TO AM-COMMENT-LINE (1)
                 MOVE '30470I TX (3.53) (CID-473)'
                                       TO AM-COMMENT-LINE (2)
              WHEN OTHER
                 DISPLAY ' FOUND A NEW STATE GET STUFF FOR IT ' AM-STATE
           END-EVALUATE

           INSPECT SL-SL-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT SL-JL-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT SL-SA-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT SL-JA-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT SL-OA-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT CSO-SL-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT CSO-JL-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT CSO-SA-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT CSO-JA-CHAR
              REPLACING ALL ' ' BY '0'

           INSPECT CSO-OA-CHAR
              REPLACING ALL ' ' BY '0'

           MOVE SL-EFF-DT (5:4)       TO WS-WORK-DATE (1:4)
           MOVE SL-EFF-DT (1:2)       TO WS-WORK-DATE (5:2)
           MOVE SL-EFF-DT (3:2)       TO WS-WORK-DATE (7:2)
           MOVE 20090701               TO WS-WORK-DATE-N
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE WS-WORK-DATE-N      TO AM-EFFECT-DT
                                          AM-PREV-EFF-DT
              MOVE DC-BIN-DATE-1       TO AM-EFFECTIVE-DT
           ELSE
              DISPLAY ' EFF DATE ERROR ON ACCT ' AM-ACCOUNT
                   '  ' SL-EFF-DT
           END-IF

           MOVE LOW-VALUES             TO AM-CNTRL-B
           MOVE HIGH-VALUES            TO AM-EXPIRATION-DT
           MOVE AM-CNTRL-B             TO AM-VG-DATE
           MOVE 99999999999            TO AM-PREV-EXP-DT

           PERFORM VARYING NT1 FROM +1 BY +1 UNTIL
              (NT1 > MNT1)
              OR (AM-ACCOUNT = WS-NT-ACCT-NO (NT1))
           END-PERFORM

           IF NT1 > MNT1
              MOVE SL-NAME             TO AM-NAME
              MOVE SPACES              TO AM-PERSON
              IF SL-NAME (31:15) NOT = SPACES
                 MOVE WS-INIT-NOTE     TO NOTE-FILE
                 MOVE AM-CONTROL-A     TO NT-ACCT-NOTE-KEY
                 MOVE +1               TO NT-LINE-SEQUENCE
                 MOVE SL-NAME          TO NT-SHIPPING-LINE
                 WRITE CID-NOTE-RECORD-OUT
                                       FROM NOTE-FILE
                 ADD 1                 TO WS-NOTE-OUTPUT-CNT
              END-IF
              DISPLAY ' CANNOT FIND NAME ' AM-ACCOUNT
      *       PERFORM ABEND-PGM
           ELSE
              MOVE WS-NT-ACCT-NAME (NT1)
                                       TO AM-NAME
              MOVE WS-NT-CONTR-NAME (NT1)
                                       TO AM-PERSON
              IF WS-NT-ACCT-NAME (NT1) (31:15) NOT = SPACES
                 MOVE WS-NT-ACCT-NAME (NT1)
                                       TO WS-WORK-NAME1
                 PERFORM 0085-FIND-DBA THRU 0085-EXIT
                 MOVE WS-INIT-NOTE     TO NOTE-FILE
                 MOVE AM-CONTROL-A     TO NT-ACCT-NOTE-KEY
                 MOVE +1               TO NT-LINE-SEQUENCE
                 MOVE WS-NT-ACCT-NAME (NT1)
                                       TO NT-SHIPPING-LINE
                 WRITE CID-NOTE-RECORD-OUT
                                       FROM NOTE-FILE
                 ADD 1                 TO WS-NOTE-OUTPUT-CNT
              END-IF
              IF WS-NT-CONTR-NAME (NT1) (31:15) NOT = SPACES
                 MOVE WS-INIT-NOTE     TO NOTE-FILE
                 MOVE AM-CONTROL-A     TO NT-ACCT-NOTE-KEY
                 MOVE +2               TO NT-LINE-SEQUENCE
                 MOVE WS-NT-CONTR-NAME (NT1)
                                       TO NT-SHIPPING-LINE
                 WRITE CID-NOTE-RECORD-OUT
                                       FROM NOTE-FILE
                 ADD 1                 TO WS-NOTE-OUTPUT-CNT
              END-IF
           END-IF

           IF SL-MADDR1 NOT = SL-PADDR1
              MOVE SL-PADDR1           TO WS-WORK-STREET
           END-IF

           PERFORM VARYING M1 FROM +35 BY -1 UNTIL
              (M1 < 1)
              OR (SL-MCITYST (M1:1) NOT = ' ')
           END-PERFORM

           PERFORM VARYING P1 FROM +35 BY -1 UNTIL
              (P1 < 1)
              OR (SL-PCITYST (P1:1) NOT = ' ')
           END-PERFORM

           IF (M1 > 1)
              AND (P1 > 1)
              IF SL-MCITYST (M1 - 1:2) NOT = SL-PCITYST (P1 - 1:2)
                 DISPLAY ' FOUND DIFF STATE ' SL-PCITYST ' '
                    SL-MCITYST
                 MOVE '?'              TO WS-WORK-STATE (1:1)
                 MOVE SL-PCITYST (P1 - 1:2)
                                       TO WS-WORK-STATE (2:2)
              END-IF
              PERFORM VARYING M1 FROM M1 BY -1 UNTIL
                 (M1 < 1)
                 OR (SL-MCITYST (M1:1) = ',')
              END-PERFORM
              PERFORM VARYING P1 FROM P1 BY -1 UNTIL
                 (P1 < 1)
                 OR (SL-PCITYST (P1:1) = ',')
              END-PERFORM
              IF (M1 > 1)
                 AND (P1 > 1)
                 IF SL-MCITYST (1:M1 - 1) NOT = SL-PCITYST (1:P1 - 1)
                    MOVE '*'           TO WS-WORK-CITY (1:1)
                    MOVE SL-PCITYST (1:P1 - 1)
                                       TO WS-WORK-CITY (2:34)
                 END-IF
              ELSE
                 DISPLAY ' SOMETHING GOING ON WITH CITY ' SL-MCITYST ' '
                    SL-PCITYST
              END-IF
           ELSE
              DISPLAY ' SOMETHING GOING ON WITH ST ' SL-MCITYST ' '
                 SL-PCITYST
           END-IF

           IF SL-MZIP (1:5) NOT = SL-PZIP (1:5)
              MOVE '+'                 TO WS-WORK-ZIP (1:1)
              MOVE SL-PZIP             TO WS-WORK-ZIP (2:9)
           END-IF

           STRING WS-WORK-STREET
                  WS-WORK-CITY
                  WS-WORK-STATE
                  WS-WORK-ZIP
              DELIMITED BY '  ' INTO AM-COMMENT-LINE (3)
           END-STRING

           MOVE SL-MADDR1              TO AM-ADDRS
      *    IF SL-MADDR1 NOT = SL-PADDR1
      *       MOVE SL-PADDR1           TO AM-COMMENT-LINE (3)
      *    END-IF

           MOVE SL-MCITYST             TO AM-CITY

           INSPECT AM-CITY
              REPLACING ALL ',' BY ' '

           MOVE SL-MZIP                TO AM-ZIP

      *    IF SL-MADDR1 NOT = SL-PADDR1
      *       MOVE SL-PADDR1           TO AM-COMMENT-LINE (3)
      *    END-IF

      *    PERFORM VARYING S4 FROM +1 BY +1 UNTIL 
      *       (S4 > UM4)
      *       OR (WS-LIC-ACCOUNT (S4) = AM-ORIG-DEALER-NO)
      *    END-PERFORM
      *
      *    IF S4 > UM4
      *       DISPLAY ' LICENSE REC NOT FOUND ' AM-ACCOUNT
      *    ELSE
      *       IF AM-STATE = 'FL'
      *          MOVE '*'              TO AM-COMMENT-LINE (5)
      *          MOVE WS-LIC-DLR-LIC-NO (S4)
      *                                TO AM-COMMENT-LINE (5) (2:15)
      *       END-IF
      *    END-IF

      *    PERFORM VARYING S4 FROM +1 BY +1 UNTIL 
      *       (S4 > UM4)
      *       OR (WS-LIC-ACCOUNT (S4) = AM-ORIG-DEALER-NO)
      *    END-PERFORM

      *    IF S4 > UM4
      *       DISPLAY ' LICENSE REC NOT FOUND ' AM-ACCOUNT
      *    ELSE
      *       IF WS-LIC-NAME1 (S4) NOT = SPACES
      *          MOVE WS-LIC-NAME1 (S4) TO AM-NAME
      *       END-IF
      *       IF WS-LIC-NAME2 (S4) NOT = SPACES
      *          MOVE WS-LIC-NAME2 (S4) TO AM-NAME
      *          MOVE WS-LIC-NAME1 (S4) TO AM-PERSON
      *          PERFORM VARYING T2 FROM +30 BY -1 UNTIL
      *             (AM-NAME (T2:1) NOT = SPACES)
      *             OR (T2 < +2)
      *          END-PERFORM
      *          IF (T2 > +1)
      *             AND (T2 < +26)
      *             ADD +1 TO T2
      *             MOVE ' DBA' TO AM-NAME (T2:4)
      *          END-IF
      *       END-IF
      *    END-IF

           MOVE SL-PHONE               TO AM-TEL-NO
           MOVE SL-TAXID               TO AM-ID-NO

           IF SL-AGT-TYPE = 'C'
              MOVE AM-ACCOUNT          TO AM-AGT (1)
              MOVE 'D'                 TO AM-COM-TYP (1)
      *       MOVE SL-SL-COMM         TO AM-L-COM (1)
      *       MOVE SL-JL-COMM         TO AM-J-COM (1)
      *       MOVE SL-JA-COMM         TO AM-A-COM (1)
              MOVE ZEROS              TO AM-L-COM (1)
              MOVE ZEROS              TO AM-J-COM (1)
              MOVE ZEROS              TO AM-A-COM (1)
           ELSE
              DISPLAY ' FIRST AGT TYPE NOT C ' WS-ORIG-UNI-ACCT
              MOVE AM-ACCOUNT          TO AM-AGT (1)
              MOVE 'D'                 TO AM-COM-TYP (1)
           END-IF

           MOVE 'S99'                  TO AM-REI-TABLE
      *    IF AM-STATE = 'GA' OR 'HI' OR 'KS' OR 'NJ'
      *          OR 'SD' OR 'TX'
      *       MOVE '1'                 TO AM-IG
      *    END-IF

      *    IF SL-ZCB-IND = 'Y'
      *       MOVE 3                   TO AM-COMM-CHARGEBACK (1)
      *       IF AM-STATE = 'FL'
      *          PERFORM VARYING S4 FROM +15 BY -1 UNTIL
      *             (AM-COMMENT-LINE (5) (S4:1) NOT = SPACES)
      *             OR (S4 < +2)
      *          END-PERFORM
      *          IF S4 > +1
      *             MOVE '90 NCB'    TO AM-COMMENT-LINE (5) (S4 + 2:6)
      *          ELSE
      *             DISPLAY ' PROBLEM WITH FL LICENSE NO ' AM-ACCOUNT
      *          END-IF
      *       ELSE
      *          MOVE '90 NCB'         TO AM-COMMENT-LINE (5)
      *       END-IF
      *    END-IF

      *     IF SL-ENTITY (1:1) NOT = 'I'
      *        MOVE 'CO'                TO SL-ENTITY
      *     END-IF
      *     MOVE SL-ENTITY             TO WS-CO-ENTITY
           IF (SL-FAX-NO (1:2) = SPACES)
              OR (SL-FAX-NO (1:3) = '111')
              MOVE SPACES              TO SL-FAX-NO
           END-IF
           MOVE SL-FAX-NO             TO WS-CO-FAX-NO

           MOVE 01                     TO AM-REMIT-TO
           
           MOVE 'SVCLIFE'              TO AM-REPORT-CODE-1
                                          AM-REPORT-CODE-2
                                          AM-REPORT-CODE-3

           IF SL-GRP-ID NOT = SPACES
              PERFORM VARYING R1 FROM +1 BY +1 UNTIL
                 (R1 > MR1)
                 OR (WS-GROUP-ID (R1) = SL-GRP-ID)
              END-PERFORM
              IF R1 <= MR1
                 MOVE WS-REPORT-CODE-2 (R1)
                                          TO AM-REPORT-CODE-2
              ELSE
                 DISPLAY ' NO REPORT CODE 2 FOR GRP ID ' SL-GRP-ID
              END-IF
           END-IF

      *  AM STATUS OF 1 IS INACTIVE
           MOVE '1'                    TO AM-STATUS

           PERFORM 0095-BUILD-COMP     THRU 0095-EXIT
           PERFORM 0090-ADD-AGENTS     THRU 0090-EXIT
           PERFORM 0078-BUILD-ERPLAN   THRU 0078-EXIT

           PERFORM VARYING R1 FROM +1 BY +1 UNTIL
              (R1 > +14)
              OR (WS-TERR-ID (R1) = WS-TM-TERR)
           END-PERFORM
           IF R1 <= +14
              MOVE WS-REPORT-CODE-1 (R1)
                                       TO AM-REPORT-CODE-1
              DISPLAY ' FOUND TM TERR ' AM-REPORT-CODE-1
           ELSE
              DISPLAY ' USING RM ' SL-ACCOUNT
              PERFORM VARYING R1 FROM +1 BY +1 UNTIL
                 (R1 > +14)
                 OR (WS-TERR-ID (R1) = WS-RM-TERR)
              END-PERFORM
              IF R1 <= +14
                 MOVE WS-REPORT-CODE-1 (R1)
                                          TO AM-REPORT-CODE-1
                 DISPLAY ' FOUND RM TERR ' AM-REPORT-CODE-1
              ELSE
                 DISPLAY ' FOUND NO TERRS ' SL-ACCOUNT
              END-IF
           END-IF

           MOVE SPACES                 TO AM-COMMENT-LINE (4)
           STRING AM-REPORT-CODE-1 AM-REPORT-CODE-2 DELIMITED
              BY SIZE INTO AM-COMMENT-LINE (4)
           END-STRING

           IF SL-REIN-TYPE = 'M'
              MOVE 'R'                 TO SL-REIN-TYPE
           END-IF
           STRING SL-REIN-TYPE SL-REIN-NO DELIMITED BY SIZE
              INTO AM-COMMENT-LINE (4) (24:5)
           END-STRING

      *    EVALUATE AM-STATE
      *       WHEN 'AZ'
      *          MOVE '04'             TO AM-CAL-TABLE
      *       WHEN 'CO'
      *          MOVE 'CR'             TO AM-CAL-TABLE
      *       WHEN 'MT'
      *          MOVE '70'             TO AM-CAL-TABLE
      *       WHEN 'NH'
      *          MOVE 'B '             TO AM-CAL-TABLE
      *       WHEN 'OR'
      *          MOVE '02'             TO AM-CAL-TABLE
      *       WHEN 'TX'
      *          MOVE 'EE'             TO AM-CAL-TABLE
      *    END-EVALUATE

      *    EVALUATE AM-STATE
      *       WHEN 'AL'
      *          MOVE 'AL (COMBO $1 NO HA) 7-21-06'
      *                                TO AM-COMMENT-LINE (1)
      *          MOVE '30470C AL Rev (CID 520)'
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN 'NC'
      *          MOVE '30470C NC Rev  (CID-502)    NP'
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN 'PA'
      *          MOVE 'PA (Combo $1-BAGE-NO HA) 7-21-06'
      *                                TO AM-COMMENT-LINE (1)
      *          MOVE '30470A PA Rev/30470C PA Rev (CID-372)'
      *                                TO AM-COMMENT-LINE (2)
      *    END-EVALUATE

           .
       0080-EXIT.
           EXIT.

       0085-FIND-DBA.

           PERFORM VARYING WN1 FROM +50 BY -1 UNTIL
              (WN1 < +1)
              OR (WS-WORK-NAME1 (WN1:1) NOT = ' ')
           END-PERFORM
           IF WN1 > +3
              IF WS-WORK-NAME1 (WN1 - 2:3) = 'DBA'
                 MOVE SPACES           TO AM-NAME
                 MOVE WS-WORK-NAME1 (1:26)
                                       TO AM-NAME (1:26)
                 MOVE 'DBA'            TO AM-NAME (28:3)
              END-IF
           END-IF

           .
       0085-EXIT.
           EXIT.

       0090-ADD-AGENTS.

           MOVE '0000668900'           TO AM-AGT (2)
           MOVE 'P'                    TO AM-COM-TYP (2)
           MOVE ZEROS                  TO AM-L-COM (2)
                                          AM-J-COM (2)
                                          AM-A-COM (2)

           MOVE SPACES                 TO WS-TM-TERR
                                          WS-RM-TERR

           PERFORM VARYING S3 FROM +1 BY +1 UNTIL
              S3 > UM3
              IF SL-ACCOUNT = WS-AGT-ACCOUNT (S3)
                 IF WS-AGENT-TYPE (S3) = 'TM'
                    MOVE WS-AGT-TERR (S3) TO WS-TM-TERR
                 END-IF
                 IF WS-AGENT-TYPE (S3) = 'RM'
                    MOVE WS-AGT-TERR (S3) TO WS-RM-TERR
                 END-IF
                 PERFORM VARYING A1 FROM +1 BY +1 UNTIL
                    (A1 > +10)
                    OR (AM-AGT (A1) = ZEROS)
                 END-PERFORM
                 IF A1 < +11
                    INSPECT WS-AGENT-SL-CHAR (S3)
                       REPLACING ALL ' ' BY '0'
                    INSPECT WS-AGENT-JL-CHAR (S3)
                       REPLACING ALL ' ' BY '0'
                    INSPECT WS-AGENT-JA-CHAR (S3)
                       REPLACING ALL ' ' BY '0'
                    MOVE WS-AGENT-NUMBER (S3)
                                       TO AM-AGT (A1)
                    MOVE 'P'           TO AM-COM-TYP (A1)
      *             MOVE WS-AGENT-SL-COMM (S3)
      *                                TO AM-L-COM (A1)
      *             MOVE WS-AGENT-JL-COMM (S3)
      *                                TO AM-J-COM (A1)
      *             MOVE WS-AGENT-JA-COMM (S3)
      *                                TO AM-A-COM (A1)
                    MOVE ZEROS         TO AM-L-COM (A1)
                    MOVE ZEROS         TO AM-J-COM (A1)
                    MOVE ZEROS         TO AM-A-COM (A1)
                    PERFORM 0100-WRITE-G-ERCOMP
                                       THRU 0100-EXIT
                 END-IF
              END-IF
           END-PERFORM
      *
      *    MOVE ZEROS                  TO WS-NO-OF-GAS
      *
      *    PERFORM VARYING A1 FROM +1 BY +1 UNTIL
      *       (A1 > +10)
      *       OR (AM-AGT (A1) = ZEROS)
      *    END-PERFORM
      *    IF A1 < +11
      *       MOVE WS-HOME-OFFICE      TO AM-AGT (A1)
      *       MOVE 'P'                 TO AM-COM-TYP (A1)
      *       MOVE A1                  TO AM-3RD-PARTY-NOTIF-LEVEL
      *       IF A1 > +1
      *          COMPUTE WS-NO-OF-GAS = A1 - 2
      *       END-IF
      *    ELSE
      *       DISPLAY ' PROBLEM WITH HOME OFFICE ' AM-ACCOUNT
      *    END-IF
      *
      *    IF AM-COMMENT-LINE (5) = SPACES
      *       MOVE 'O - '              TO AM-COMMENT-LINE (5)
      *       MOVE WS-NO-OF-GAS        TO AM-COMMENT-LINE (5) (5:1)
      *    ELSE
      *       MOVE 'O - '              TO AM-COMMENT-LINE (5) (8:4)
      *       MOVE WS-NO-OF-GAS        TO AM-COMMENT-LINE (5) (12:1)
      *    END-IF

           .
       0090-EXIT.
           EXIT.

       0095-BUILD-COMP.

           MOVE WS-INIT-COMP           TO COMPENSATION-MASTER
           
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING

           IF AM-REMIT-TO = ZEROS OR SPACES OR LOW-VALUES
              MOVE 01                  TO AM-REMIT-TO
           END-IF

           MOVE AM-AGT (01)            TO CO-RESP-NO
           MOVE AM-AGT (01)            TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE
           MOVE 'Y'                    TO CO-BALANCE-CONTROL
           MOVE AM-NAME                TO CO-ACCT-NAME 
           MOVE AM-PERSON              TO CO-MAIL-NAME 
           MOVE AM-ADDRS               TO CO-ADDR-1 
           MOVE AM-CITY                TO CO-ADDR-3 
           MOVE AM-ZIP                 TO CO-ZIP 
           MOVE AM-CSR-CODE            TO CO-CSR-CODE 
           MOVE AM-TEL-NO              TO CO-TELEPHONE 
           MOVE AM-ID-NO               TO CO-SOC-SEC 
           MOVE 'B'                    TO CO-BILL-SW
           
      *     IF WS-CO-ENTITY = 'CO'
              MOVE 'N'                 TO CO-CSO-1099
      *     ELSE
      *        MOVE 'Y'                 TO CO-CSO-1099
      *     END-IF
           MOVE WS-CO-FAX-NO           TO CO-FAXNO
           WRITE COMP-RECORD-OUT       FROM COMPENSATION-MASTER
           ADD +1 TO WS-A-COMP-RECS
           MOVE COMPENSATION-MASTER    TO WS-SAVE-COMP

           .
       0095-EXIT.
           EXIT.

       0100-WRITE-G-ERCOMP.

           MOVE WS-INIT-COMP           TO COMPENSATION-MASTER
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING
           MOVE LOW-VALUES             TO CO-ACCOUNT
           MOVE AM-AGT (A1)            TO CO-RESP-NO
           MOVE 'G'                    TO CO-TYPE

           MOVE WS-AGENT-NAME (S3)     TO CO-ACCT-NAME
           MOVE WS-AGENT-ADDR1 (S3)    TO CO-ADDR-1
           MOVE WS-AGENT-ADDR2 (S3)    TO CO-ADDR-2
           MOVE WS-AGENT-ADDR3 (S3)    TO CO-ADDR-3

           INSPECT CO-ADDR-3
              REPLACING ALL ',' BY ' '

           MOVE WS-AGENT-ZIP   (S3)    TO CO-ZIP
           MOVE WS-AGENT-PHONE (S3)    TO CO-TELEPHONE
           MOVE WS-AGENT-FAX-NO (S3)   TO CO-FAXNO
           MOVE WS-AGENT-TAX-ID (S3)   TO CO-SOC-SEC
           MOVE 'Y'                    TO CO-BALANCE-CONTROL
      *    IF WS-AGENT-ENTITY (S3) (1:2) = 'CO'
              MOVE 'N'                 TO CO-CSO-1099
      *    ELSE
      *       MOVE 'Y'                 TO CO-CSO-1099
      *    END-IF

           MOVE COMPENSATION-MASTER    TO WS-SAVE-COMP
           WRITE COMP-RECORD-OUT       FROM COMPENSATION-MASTER
           ADD +1 TO WS-G-COMP-RECS

           .
       0100-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
