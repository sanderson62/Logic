       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS0631.
       AUTHOR.        SUZAN VUKOV.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
022604* 022604  2003080800002    SMVA  NEW PROGRAM FOR SECURE PAY    
102704* 102704  2004101100006    PEMA  MINOR COSMETIC CHANGES
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
102805* 102805  CR2005080300008  AJRA  ADD CLAIM ACTIVITY
110305* 110305  CR2005080300008  AJRA  COMPILER CHANGES IN VER 4
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

110305     SELECT ERCOMP-FILE      ASSIGN TO ERCOMP
                                   ORGANIZATION INDEXED
                                   ACCESS DYNAMIC
                                   RECORD KEY CO-CONTROL-PRIMARY
                                   FILE STATUS ERCOMP-STATUS.

110305     SELECT ERACCT-FILE      ASSIGN TO ERACCT
                                   ORGANIZATION INDEXED
                                   ACCESS DYNAMIC
                                   RECORD KEY AM-CONTROL-PRIMARY
                                   FILE STATUS ERACCT-STATUS.

102805     SELECT ELCRTT-FILE      ASSIGN TO ELCRTT
102805                             ORGANIZATION INDEXED
102805                             ACCESS DYNAMIC
102805                             RECORD KEY CS-CONTROL-PRIMARY
102805                             FILE STATUS ELCRTT-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT SORT-WORK        ASSIGN TO SYS001.

           SELECT PRT-OUT          ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSEXT01.

110305 FD  ERCOMP-FILE.
           COPY ERCCOMP.

110305 FD  ERACCT-FILE.
           COPY ERCACCT.

102805 FD  ELCRTT-FILE.
102805     COPY ELCCRTT.

       FD  DISK-DATE
           COPY ELCDTEFD.

       SD  SORT-WORK.
       01  SORT-REC.
           12  SORT-KEY             PIC  X(41).
           12  SORT-DETAIL          PIC  X(73).
     
       FD  PRT-OUT
           COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   ECS0631  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  S1                          PIC S999 VALUE +0 COMP-3.

       01  WS-LINE-COUNT            COMP-3  PIC S9(03) VALUE +0.
       01  WS-LINE-COUNT-MAX        COMP-3  PIC S9(03) VALUE +55.
       01  WS-PAGE                  COMP-3  PIC S9(05) VALUE +0.
       01  WS-RELEASE-SORT-COUNT    COMP-3  PIC S9(05) VALUE +0.
       01  WS-RETURN-SORT-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-ISSUE-COUNT      COMP-3  PIC S9(05) VALUE +0.
       01  WS-DEALER-ISSUE-COUNT    COMP-3  PIC S9(05) VALUE +0.
       01  WS-ALL-ISSUE-COUNT       COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-CANCEL-COUNT     COMP-3  PIC S9(05) VALUE +0.
       01  WS-DEALER-CANCEL-COUNT   COMP-3  PIC S9(05) VALUE +0.
       01  WS-ALL-CANCEL-COUNT      COMP-3  PIC S9(05) VALUE +0.
102805 01  WS-BANK-CLAIM-COUNT      COMP-3  PIC S9(05) VALUE +0.
102805 01  WS-DEALER-CLAIM-COUNT    COMP-3  PIC S9(05) VALUE +0.
102805 01  WS-ALL-CLAIM-COUNT       COMP-3  PIC S9(05) VALUE +0.
       01  WS-BANK-BANK-FEE-TOTAL   COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-DEALER-BANK-FEE-TOTAL COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-ALL-BANK-FEE-TOTAL    COMP-3  PIC S9(07)V99 VALUE +0.
102805 01  WS-BANK-CLAIM-TOTAL      COMP-3  PIC S9(07)V99 VALUE +0.
102805 01  WS-DEALER-CLAIM-TOTAL    COMP-3  PIC S9(07)V99 VALUE +0.
102805 01  WS-ALL-CLAIM-TOTAL       COMP-3  PIC S9(07)V99 VALUE +0.
       01  WS-CNC-FACT              COMP-3  PIC S9(03)V9(07) VALUE +0.

       01  WS-EOF-SW                        PIC  X(01) VALUE SPACE.
           88  END-OF-EXTR                             VALUE 'Y'.

       01  WS-EOF-SW2                       PIC  X(01) VALUE SPACE.
           88  END-OF-SORT-FILE                        VALUE 'Y'.

       01  EXTR-RECS-IN                     PIC  9(09) VALUE ZEROS.
       01  SUB1                             PIC S9(05) VALUE +0 COMP-3.
       01  PGM-SUB                   COMP-3 PIC S9(03) VALUE +0.
       01  ERCOMP-STATUS                    PIC  X(02) VALUE SPACES.
       01  ERACCT-STATUS                    PIC  X(02) VALUE SPACES.
       01  ELCRTT-STATUS                    PIC  X(02) VALUE SPACES.

       01  WS-BENEFIT-CODE-SW               PIC  X(01) VALUE SPACE.
           88  AH-BEN-CODE-FOUND                       VALUE 'Y'.

       01  WS-RECORD-FOUND-SW               PIC  X(01) VALUE SPACE.
           88  GOT-MOST-CURRENT                        VALUE 'Y'.

       01  WS-FIRST-TIME-SW                 PIC  X(01) VALUE 'Y'.
           88  FIRST-TIME                              VALUE 'Y'.
           88  NOT-FIRST-TIME                          VALUE 'N'.

       01  WS-CHNG-IN-BANK-SW               PIC  X(01) VALUE 'Y'.
           88  NEW-BANK                                VALUE 'Y'.
           88  SAME-BANK                               VALUE 'N'.

       01  WS-ABEND-FIELDS.
           05  WS-RETURN-CODE               PIC S9(04) VALUE ZERO.
           05  WS-ZERO                      PIC S9(01) VALUE ZERO.
           05  WS-ABEND-MESSAGE             PIC  X(80) VALUE SPACES.
           05  WS-ABEND-FILE-STATUS         PIC  X(02) VALUE ZERO.

102805 01  WS-WORK-AREA.
102805     05  WS-AGT-TYPE                  PIC X(1).
102805     05  WS-AGT                       PIC X(10).
102805     05  WS-EFF-DATE                  PIC 9(11).
102805     05  FILLER  REDEFINES WS-EFF-DATE.
102805         10  FILLER                   PIC 9(3).
102805         10  WS-EFF-CEN               PIC 9(2).
102805         10  WS-EFF-YEAR              PIC 9(2).
102805         10  WS-EFF-MONTH             PIC 9(2).
102805         10  WS-EFF-DAY               PIC 9(2).
102805
      ***** SAVE AREA  *****  
       01  WS-SAVE-SORT-KEY-27              PIC  X(27).
       01  WS-PREV-BANK-NO                  PIC  X(10).
       01  WS-PREV-ACCOUNT-NO               PIC  X(10).
       01  WS-PREV-REC-TYPE                 PIC  X(02).
       01  WS-PREV-REFUND-OPT-FLG           PIC  X(01).
       01  WS-SAVE-AM-CONTROL-A             PIC  X(19).
       01  WS-SAVE-AM-NAME                  PIC  X(30).


      ***** SORT WORK AREA  *****  
       01  WS-SORT-REC.
           05  WS-SORT-KEY.
               10  WS-SORT-CARRIER          PIC  X(01) VALUE SPACE.
               10  WS-SORT-GROUP            PIC  X(06) VALUE SPACES.
               10  WS-SORT-BANK-NO          PIC  X(10) VALUE SPACES.
               10  WS-SORT-ACCOUNT          PIC  X(10) VALUE SPACES.
               10  WS-SORT-REC-TYPE         PIC  X(02) VALUE SPACES.
                   88  REF-ISSUE                       VALUE '01'.
                   88  REF-CANCEL                      VALUE '02'.
                   88  NON-REF-ISSUE                   VALUE '03'.
                   88  NON-REF-CANCEL                  VALUE '04'.
102805             88  SRT-CLAIM                       VALUE '05'.                   
               10  WS-SORT-REFUND-OPT-FLG   PIC  X(01) VALUE SPACE.
                   88  NOT-REFUNDABLE-SECTION          VALUE ' '.
                   88  REFUNDABLE-SECTION              VALUE '*'.
               10  WS-SORT-CERT             PIC  X(11) VALUE SPACES.
           05  WS-SORT-INSURED-NAME.
               10  WS-SORT-LNAME            PIC  X(15) VALUE SPACES.
               10  WS-SORT-FNAME            PIC  X(10) VALUE SPACES.
               10  WS-SORT-INIT             PIC  X(01) VALUE SPACES.
           05  WS-SORT-CERT-EFF-DT          PIC  9(11) VALUE 0.
           05  WS-SORT-AGE                  PIC  9(02) VALUE 0.
           05  WS-SORT-AH-TERM              PIC S9(03) VALUE +0.
           05  WS-SORT-AH-TYPE              PIC  X(03) VALUE SPACES.
102805     05  WS-SORT-CLM-TYPE   REDEFINES 
102805         WS-SORT-AH-TYPE.
102805         10  WS-SORT-CLM-TYP          PIC  X(01).
102805         10  FILLER                   PIC  X(02).           
           05  WS-SORT-PRM-AMT       COMP-3 PIC S9(9)V99 VALUE +0.
102805     05  WS-SORT-CLM-AMT    REDEFINES 
102805         WS-SORT-PRM-AMT       COMP-3 PIC S9(9)V99.
           05  WS-SORT-AH-CAN-DT            PIC  9(11) VALUE 0.
           05  WS-SORT-AH-REFUND     COMP-3 PIC S9(07)V99 VALUE +0.
           05  WS-SORT-BANK-FEE      COMP-3 PIC S9(05)V99 VALUE +0.
           05  WS-SORT-STATE                PIC  X(02) VALUE SPACES.

      ***** REPORT LAYOUT AREA  *****  
       01  WS-DETAIL1A.
           05 FILLER                       PIC  X(01)  VALUE ' '.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1A-INSURED-NAME          PIC  X(26)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1A-CERT                  PIC  X(10)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1A-CERT-EFF-DT.
              10  WS-D1A-CERT-EFF-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1A-CERT-EFF-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1A-CERT-EFF-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1A-AGE                   PIC  X(02)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1A-AH-TERM               PIC  ZZ9.
           05 FILLER                       PIC  X(06)  VALUE SPACES.
           05 WS-D1A-AH-TYPE               PIC  X(03)  VALUE SPACES.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1A-PRM-AMT               PIC  Z,ZZZ,ZZZ.99-.
           05 FILLER                       PIC  X(08)  VALUE SPACES.
           05 WS-D1A-BANK-FEE              PIC  Z,ZZZ.99-.
           05 FILLER                       PIC  X(24)  VALUE SPACES.
 
       01  WS-DETAIL1B.
           05 FILLER                       PIC  X(01)  VALUE ' '.
           05 FILLER                       PIC  X(02)  VALUE SPACES.
           05 WS-D1B-INSURED-NAME          PIC  X(26)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1B-CERT                  PIC  X(10)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-CERT-EFF-DT.
              10  WS-D1B-CERT-EFF-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-EFF-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-EFF-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-CERT-CANCEL-DT.
              10  WS-D1B-CERT-CAN-MM       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-CAN-DD       PIC  X(02)  VALUE SPACES.
              10  FILLER                   PIC  X(01)  VALUE '/'.
              10  WS-D1B-CERT-CAN-CCYY     PIC  X(04)  VALUE SPACES.
           05 FILLER                       PIC  X(03)  VALUE SPACES.
           05 WS-D1B-AH-TYPE               PIC  X(03)  VALUE SPACES.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
102704     05 WS-D1B-PRM-AMT               PIC  Z,ZZZ,ZZZ.99-.
102704     05 FILLER                       PIC  X(04).
           05 WS-D1B-REFUND-AMT            PIC  Z,ZZZ,ZZZ.99-.
           05 FILLER                       PIC  X(04)  VALUE SPACES.
           05 WS-D1B-REFUND-BKFEE          PIC  Z,ZZZ.99-.
           05 FILLER                       PIC  X(28)  VALUE SPACES.
102805
102805 01  WS-DETAIL1C.
102805     05 FILLER                       PIC  X(01)  VALUE ' '.
102805     05 FILLER                       PIC  X(02)  VALUE SPACES.
102805     05 WS-D1C-INSURED-NAME          PIC  X(26)  VALUE SPACES.
102805     05 FILLER                       PIC  X(04)  VALUE SPACES.
102805     05 WS-D1C-CERT                  PIC  X(10)  VALUE SPACES.
102805     05 FILLER                       PIC  X(03)  VALUE SPACES.
102805     05 WS-D1C-CERT-EFF-DT.
102805        10  WS-D1C-CERT-EFF-MM       PIC  X(02)  VALUE SPACES.
102805        10  FILLER                   PIC  X(01)  VALUE '/'.
102805        10  WS-D1C-CERT-EFF-DD       PIC  X(02)  VALUE SPACES.
102805        10  FILLER                   PIC  X(01)  VALUE '/'.
102805        10  WS-D1C-CERT-EFF-CCYY     PIC  X(04)  VALUE SPACES.
102805     05 FILLER                       PIC  X(06)  VALUE SPACES.
102805     05 WS-D1C-CLM-L-A               PIC  X(06)  VALUE SPACES.
102805     05 FILLER                       PIC  X(01)  VALUE SPACES.
102805     05 WS-D1C-CLM-MSG               PIC  X(28)  VALUE SPACES.
102805     05 FILLER                       PIC  X(02)  VALUE SPACES.
102805     05 WS-D1C-CLM-AMT               PIC  Z,ZZZ,ZZZ.99-.
102805     05 FILLER                       PIC  X(21)  VALUE SPACES.

       01  WS-HD1.
           05  FILLER                      PIC  X(01)  VALUE '1'.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
           05  WS-HD1-TITLE                PIC  X(68)  VALUE
               'BANK FEE RECONCILIATION DETAIL'.
           05  FILLER                      PIC  X(07)  VALUE 'ECS0631'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
     
       01  WS-HD2.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
           05  WS-HD2-CO                   PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(38)  VALUE SPACES.
           05  WS-HD2-RUN-DT               PIC  X(08)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
     
       01  WS-HD3.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(56)  VALUE SPACES.
           05  WS-HD3-ALPHA-DT             PIC  X(18)  VALUE SPACES.
           05  FILLER                      PIC  X(44)  VALUE SPACES.
           05  FILLER                      PIC  X(05)  VALUE 'PAGE'.
           05  WS-HD3-PAGE                 PIC  ZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
     
       01  WS-HD4-1.
           05  FILLER                      PIC  X(01)  VALUE '0'.
           05  FILLER                      PIC  X(24)  VALUE
               'FINANCIAL INSTITUTION - '.
           05  FILLER                      PIC  X(02)  VALUE SPACES.
           05  WS-HD4-1-BANK-NAME          PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  XX     VALUE SPACES.
           05  WS-HD4-1-CARRIER            PIC  X(01)  VALUE SPACES.
           05  WS-HD4-1-GROUP              PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  WS-HD4-1-BANK-NO            PIC  X(10)  VALUE SPACES.
           05  FILLER                      PIC  X(50)  VALUE SPACES.
     
       01  WS-HD4-2.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-2-BANK-ADDRESS1      PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-3.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-3-BANK-ADDRESS2      PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-4.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-4-BANK-ADDRESS3      PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD4-5.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(26)  VALUE SPACES.
           05  WS-HD4-5-BANK-ADDRESS4      PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(71)  VALUE SPACES.
     
       01  WS-HD5.
           05  FILLER                      PIC  X(01)  VALUE '0'.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               'DEALER NO. - '.
           05  WS-HD5-ACCT-NO              PIC  X(10)  VALUE SPACES.
           05  FILLER                      PIC  X(02)  VALUE SPACES.
           05  WS-HD5-ACCT-NAME            PIC  X(30)  VALUE SPACES.
           05  FILLER                      PIC  X(74)  VALUE SPACES.

       01  WS-HD6A.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(33)  VALUE SPACES.
           05  FILLER                      PIC  X(54)  VALUE
           'N O N - R E F U N D A B L E    N E W   B U S I N E S S'.
           05  FILLER                      PIC  X(11)  VALUE
               '    F E E S'.
           05  FILLER                      PIC  X(34)  VALUE SPACES.
      
       01  WS-HD6B.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(54)  VALUE SPACES.
           05  FILLER                      PIC  X(25)  VALUE
               'C A N C E L L A T I O N S'.
           05  FILLER                      PIC  X(53)  VALUE SPACES.

       01  WS-HD6C.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(37)  VALUE SPACES.
           05  FILLER                      PIC  X(57)  VALUE
           'R E F U N D A B L E    N E W   B U S I N E S S    F E E S'.
           05  FILLER                      PIC  X(38)  VALUE SPACES.
102805
102805 01  WS-HD6D.
102805     05  FILLER                      PIC  X(01)  VALUE ' '.
102805     05  FILLER                      PIC  X(52)  VALUE SPACES.
102805     05  FILLER                      PIC  X(28)  VALUE
102805         'C L A I M    A C T I V I T Y'.
102805     05  FILLER                      PIC  X(52)  VALUE SPACES.

       01  WS-HD7A.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(33)  VALUE SPACES.
           05  FILLER                      PIC  X(08)  VALUE
               'CONTRACT'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(09)  VALUE
               'EFFECTIVE'. 
           05  FILLER                      PIC  X(78)  VALUE SPACES.
 
       01  WS-HD7B.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(33)  VALUE SPACES.
           05  FILLER                      PIC  X(08)  VALUE
               'CONTRACT'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(09)  VALUE
               'EFFECTIVE'.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE
               'CANCEL'.
           05  FILLER                      PIC  X(34)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE 
               'REFUND'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(17)  VALUE
               'REFUNDED ADDENDUM'.
           05  FILLER                      PIC  X(10)  VALUE SPACES.
     
       01  WS-HD8A.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'NAME'.
           05  FILLER                      PIC  X(23)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE
               'NUMBER'.
           05  FILLER                      PIC  X(08)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'DATE'.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(03)  VALUE
               'AGE'.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'TERM'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'TYPE'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(12)  VALUE
      *        'FACE AMOUNT'.
               'LUMP SUM FEE'.
           05  FILLER                      PIC  X(06)  VALUE SPACES.
           05  FILLER                      PIC  X(12)  VALUE
               'ADDENDUM FEE'.
           05  FILLER                      PIC  X(22)  VALUE SPACES.

       01  WS-HD8B.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'NAME'.
           05  FILLER                      PIC  X(23)  VALUE SPACES.
           05  FILLER                      PIC  X(06)  VALUE
               'NUMBER'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'DATE'.
           05  FILLER                      PIC  X(10)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'DATE'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'TYPE'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(12)  VALUE
               'LUMP SUM FEE'.
           05  FILLER                      PIC  X(04)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               '    AMOUNT   '.
           05  FILLER                      PIC  X(05)  VALUE SPACES.
           05  FILLER                      PIC  X(04)  VALUE
               'FEES'.
           05  FILLER                      PIC  X(30)  VALUE SPACES.
102805     
102805 01  WS-HD8C.
102805     05  FILLER                      PIC  X(01)  VALUE ' '.
102805     05  FILLER                      PIC  X(07)  VALUE SPACES.
102805     05  FILLER                      PIC  X(04)  VALUE
102805*         'NAME'.
102805          SPACES.
102805     05  FILLER                      PIC  X(23)  VALUE SPACES.
102805     05  FILLER                      PIC  X(06)  VALUE
102805         'NUMBER'.
102805     05  FILLER                      PIC  X(08)  VALUE SPACES.
102805     05  FILLER                      PIC  X(04)  VALUE
102805         'DATE'.
102805     05  FILLER                      PIC  X(10)  VALUE SPACES.
102805     05  FILLER                      PIC  X(21)  VALUE
102805         'TYPE OF CLAIM PAYMENT'.
102805     05  FILLER                      PIC  X(10)  VALUE SPACES.
102805     05  FILLER                      PIC  X(12)  VALUE 
102805         'CLAIM AMOUNT'.
102805     05  FILLER                      PIC  X(27)  VALUE SPACES.
      
       01  WS-BANK-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(32)  VALUE
               '* FINANCIAL INSTITUTION TOTALS *'.
           05  FILLER                      PIC  X(3)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-BTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-BTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(32)  VALUE
               'NET FINANCIAL INSTITUTION FEES: '.
           05  WS-BTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC  X(8)  VALUE SPACES.
102805
102805 01  WS-BANK-CLAIM-TOTAL-LINE.
102805     05  FILLER                      PIC  X(01)  VALUE ' '.
102805     05  FILLER                      PIC  X(32)  VALUE SPACES.
102805     05  FILLER                      PIC  X(03)  VALUE SPACES.
102805     05  FILLER                      PIC  X(13)  VALUE
102805         'CLAIM COUNT: '.
102805     05  WS-BTOT-CLAIM-COUNT         PIC  ZZZ,ZZ9.
102805     05  FILLER                      PIC  X(03)  VALUE SPACES.
102805     05  FILLER                      PIC  X(14)  VALUE
102805         'CLAIM AMOUNT: '.
102805     05  WS-BTOT-CLAIM-AMOUNT        PIC  Z,ZZZ,ZZ9.99-.
102805     05  FILLER                      PIC  X(47)  VALUE SPACES.


       01  WS-DEALER-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(19)  VALUE
               '** DEALER TOTALS **'.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-DTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-DTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(03)  VALUE SPACES.
           05  FILLER                      PIC  X(32)  VALUE
               'NET FINANCIAL INSTITUTION FEES: '.
           05  WS-DTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC  X(8)  VALUE SPACES.
102805
102805 01  WS-DEALER-CLAIM-TOTAL-LINE.
102805     05  FILLER                      PIC  X(01)  VALUE ' '.
102805     05  FILLER                      PIC  X(19)  VALUE SPACES.
102805     05  FILLER                      PIC  X(03)  VALUE SPACES.
102805     05  FILLER                      PIC  X(13)  VALUE
102805         'CLAIM COUNT: '.
102805     05  WS-DTOT-CLAIM-COUNT         PIC  ZZZ,ZZ9.
102805     05  FILLER                      PIC  X(03)  VALUE SPACES.
102805     05  FILLER                      PIC  X(14)  VALUE
102805         'CLAIM AMOUNT: '.
102805     05  WS-DTOT-CLAIM-AMOUNT        PIC  Z,ZZZ,ZZ9.99-.
102805     05  FILLER                      PIC  X(60)  VALUE SPACES.


       01  WS-GRAND-TOTAL-LINE.
           05  FILLER                      PIC  X(01)  VALUE '-'.
           05  FILLER                      PIC  X(21)  VALUE
               '*** GRAND  TOTALS ***'.
           05  FILLER                      PIC  X(07)  VALUE SPACES.
           05  FILLER                      PIC  X(13)  VALUE
               'ISSUE COUNT: '.
           05  WS-GTOT-ISSUE-COUNT         PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(05)  VALUE SPACES.
           05  FILLER                      PIC  X(14)  VALUE
               'CANCEL COUNT: '.
           05  WS-GTOT-CANCEL-COUNT        PIC  ZZZ,ZZ9.
           05  FILLER                      PIC  X(05)  VALUE SPACES.
           05  FILLER                      PIC  X(32)  VALUE
               'NET FINANCIAL INSTITUTION FEES: '.
           05  WS-GTOT-NET-BANK-FEES       PIC  Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC  X(8)  VALUE SPACES.
102805
102805 01  WS-GRAND-CLAIM-TOTAL-LINE.
102805     05  FILLER                      PIC  X(01)  VALUE ' '.
102805     05  FILLER                      PIC  X(21)  VALUE SPACES.
102805     05  FILLER                      PIC  X(07)  VALUE SPACES.
102805     05  FILLER                      PIC  X(13)  VALUE
102805         'CLAIM COUNT: '.
102805     05  WS-GTOT-CLAIM-COUNT         PIC  ZZZ,ZZ9.
102805     05  FILLER                      PIC  X(05)  VALUE SPACES.
102805     05  FILLER                      PIC  X(14)  VALUE
102805         'CLAIM AMOUNT: '.
102805     05  WS-GTOT-CLAIM-AMOUNT        PIC  Z,ZZZ,ZZ9.99-.
102805     05  FILLER                      PIC  X(52)  VALUE SPACES.


       01  WS-HDR-DASH-LINE.
           05  FILLER                      PIC  X(01)  VALUE ' '.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.
           05  FILLER                      PIC  X(44)  VALUE
               '--------------------------------------------'.


       COPY ELCDATE.
       COPY ELCDTECX.
       COPY ELCDTEVR.

       PROCEDURE DIVISION.

      ******************************************************************
      ***        D A T E   C A R D   L O A D   R O U T I N E         ***
      ******************************************************************
     
           COPY ELCDTERX.
      ******************************************************************

           PERFORM 0400-OPEN-FILES       THRU 0400-EXIT
           PERFORM 0600-INITIALIZE       THRU 0600-EXIT

           SORT SORT-WORK ASCENDING KEY  SORT-KEY
               INPUT PROCEDURE 
                   0100-INPUT-ROUTINE    THRU 0100-EXIT
               OUTPUT PROCEDURE 
                   0200-OUTPUT-ROUTINE   THRU 0200-EXIT
      
           IF SORT-RETURN NOT = ZERO
               MOVE 'SORT FAILED'        TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN          TO WS-RETURN-CODE
               PERFORM ABEND-PGM         THRU APS-EXIT
           END-IF

           PERFORM 0500-CLOSE-FILES      THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXTR-RECS-IN
           DISPLAY ' SORT RECORDS RELEASED '  WS-RELEASE-SORT-COUNT
           DISPLAY ' SORT RECORDS RETURNED '  WS-RETURN-SORT-COUNT
           DISPLAY ' WS LINE COUNT '  WS-LINE-COUNT
         
           GOBACK

           .
       0100-INPUT-ROUTINE.
            
           PERFORM 0110-PROCESS-EXTR     THRU 0110-EXIT
               UNTIL END-OF-EXTR

           .
       0100-EXIT.
           EXIT.


       0110-PROCESS-EXTR.

           READ EXTR-FILE-IN
               AT END
                   SET END-OF-EXTR       TO TRUE
                   GO TO 0110-EXIT
           END-READ

           ADD +1                        TO EXTR-RECS-IN


           IF DE-REIN = 'R'
               GO TO 0110-EXIT
           END-IF


TEST  *    IF DE-CANCEL
TEST  *        DISPLAY 'CANCEL'
TEST  *    END-IF

           IF DE-ISSUE OR DE-CANCEL      
102805       OR DE-CLAIM           
               CONTINUE
           ELSE
               GO TO 0110-EXIT
           END-IF

           MOVE DE-AH-TYPE               TO CLAS-LOOK
     
TEST  *    DISPLAY 'CLAS LOOK ' CLAS-LOOK
           IF CLAS-LOOK = '00'
               GO TO 0110-EXIT
           END-IF     
     
           IF CLAS-STARTA = ZERO
               DISPLAY 'CLAS-STARTA = ZERO'
               PERFORM ABEND-PGM         THRU APS-EXIT
           ELSE
               MOVE CLAS-STARTA          TO CLAS-INDEXA
           END-IF
     
           MOVE SPACE                    TO WS-BENEFIT-CODE-SW
           PERFORM 0150-FIND-AH-BEN-CODE THRU 0150-EXIT
               UNTIL AH-BEN-CODE-FOUND
     
      ****** BENEFIT CATEGORY OF G INDICATES SECURE PAY BUSINESS
092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) NOT = 'G' AND 'L'
               GO TO 0110-EXIT
           END-IF

      ****** ALL SECURE PAY BANKS ARE SET UP ON THE FIRST B TYP  

102805     IF DE-CLAIM
102805         PERFORM 0160-FIND-CLAIM-AGT-TYPE THRU 0160-EXIT
102805     ELSE
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +10)
              OR (DE-AGT-TYPE (S1) = 'B')
           END-PERFORM
102805     END-IF

102805     IF DE-CLAIM
102805         IF WS-AGT-TYPE = 'B'
102805             CONTINUE
102805         ELSE
102805             GO TO 0110-EXIT
102805         END-IF
102805     ELSE
           IF (S1 NOT > +10)
              AND (DE-AGT-TYPE (S1) = 'B')
              CONTINUE
           ELSE
               GO TO 0110-EXIT
           END-IF
102805     END-IF
            
           INITIALIZE WS-SORT-REC
           MOVE DE-CARRIER               TO WS-SORT-CARRIER    
           MOVE DE-GROUPING              TO WS-SORT-GROUP
102805     IF DE-CLAIM
102805         MOVE WS-AGT               TO WS-SORT-BANK-NO
102805     ELSE
           MOVE DE-AGT (S1)              TO WS-SORT-BANK-NO
102805     END-IF
           MOVE DE-ACCOUNT               TO WS-SORT-ACCOUNT
           MOVE DE-CERT                  TO WS-SORT-CERT

           MOVE DE-EFF                   TO WS-SORT-CERT-EFF-DT
           MOVE DE-LNAME                 TO WS-SORT-LNAME      
           MOVE DE-FNAME                 TO WS-SORT-FNAME      
           MOVE DE-INIT                  TO WS-SORT-INIT       
           MOVE DE-AGE                   TO WS-SORT-AGE        
           MOVE DE-AH-TERM               TO WS-SORT-AH-TERM
           MOVE DE-AH-TYPE               TO WS-SORT-AH-TYPE
           MOVE DE-STATE                 TO WS-SORT-STATE    

102805     IF DE-CLAIM
102805         MOVE '05'                 TO WS-SORT-REC-TYPE
102805         MOVE SPACES               TO WS-SORT-CLM-TYPE
102805         MOVE DE-TYPE              TO WS-SORT-CLM-TYP
102805         MOVE DE-CLAIM-AMT         TO WS-SORT-CLM-AMT
102805         MOVE SPACES               TO WS-SORT-REFUND-OPT-FLG
102805         RELEASE SORT-REC          FROM WS-SORT-REC
102805         ADD +1                    TO WS-RELEASE-SORT-COUNT
102805         GO TO 0110-EXIT
102805     END-IF
    
      *   REF-ISSUE                       VALUE '01'.
      *   REF-CANCEL                      VALUE '02'.
      *   NON-REF-ISSUE                   VALUE '03'.
      *   NON-REF-CANCEL                  VALUE '04'.


           IF DE-ISSUE
               MOVE '01'                 TO WS-SORT-REC-TYPE
               COMPUTE WS-SORT-BANK-FEE = DE-A-PC (S1) * 1000
102704*        MOVE DE-LF-BEN            TO WS-SORT-LOAN-AMT
102704         MOVE DE-AH-PRM            TO WS-SORT-PRM-AMT
           ELSE
               MOVE '02'                 TO WS-SORT-REC-TYPE
102704         MOVE DE-AH-PRM            TO WS-SORT-PRM-AMT
               MOVE DE-AH-CANC-DTE       TO WS-SORT-AH-CAN-DT
               MOVE DE-AH-RFND           TO WS-SORT-AH-REFUND
               COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
               COMPUTE WS-SORT-BANK-FEE = DE-A-PC (S1) * WS-CNC-FACT 
                                          * 1000
           END-IF


      *  REFUND TYPE OF 'G' IS "NON REFUNDABLE"

           IF CLAS-I-REFUND-METHOD (CLAS-INDEXA) = 'G'
              IF DE-ISSUE
                 MOVE '03'             TO WS-SORT-REC-TYPE
              ELSE
                 MOVE '04'             TO WS-SORT-REC-TYPE
              END-IF
           END-IF

           MOVE SPACES                 TO WS-SORT-REFUND-OPT-FLG
           RELEASE SORT-REC            FROM WS-SORT-REC
           ADD +1                      TO WS-RELEASE-SORT-COUNT

           .
       0110-EXIT.
           EXIT.


       0150-FIND-AH-BEN-CODE.

           IF CLAS-INDEXA GREATER CLAS-MAXA
               DISPLAY 'AH BENEFIT ' CLAS-LOOK ' NOT ON FILE'
               PERFORM ABEND-PGM          THRU APS-EXIT
           END-IF
     
           IF CLAS-I-BEN (CLAS-INDEXA) = CLAS-LOOK
               SET AH-BEN-CODE-FOUND   TO TRUE
           ELSE
               ADD +1                  TO  CLAS-INDEXA
           END-IF

           .
       0150-EXIT.
           EXIT.

102805
102805 0160-FIND-CLAIM-AGT-TYPE.
102805
102805     MOVE DE-COMPANY-CD           TO  CS-COMPANY-CD
102805     MOVE DE-CARRIER              TO  CS-CARRIER
102805     MOVE DE-GROUPING             TO  CS-GROUPING
102805     MOVE DE-STATE                TO  CS-STATE
102805     MOVE DE-ACCOUNT              TO  CS-ACCOUNT
102805     MOVE '3'                     TO  DC-OPTION-CODE
102805     MOVE DE-EFF                  TO  WS-EFF-DATE
102805     MOVE WS-EFF-CEN              TO  DC-ALPHA-CEN-N
102805     MOVE WS-EFF-YEAR             TO  DC-YMD-YEAR
102805     MOVE WS-EFF-MONTH            TO  DC-YMD-MONTH
102805     MOVE WS-EFF-DAY              TO  DC-YMD-DAY
102805     PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
102805     MOVE DC-BIN-DATE-1           TO  CS-CERT-EFF-DT
102805     MOVE DE-CERT                 TO  CS-CERT-NO
102805     MOVE 'A'                     TO  CS-TRAILER-TYPE
102805
102805     READ ELCRTT-FILE
102805
102805     EVALUATE ELCRTT-STATUS
102805         WHEN '00'
102805             PERFORM VARYING S1 FROM +1 BY +1 UNTIL
102805                (S1 > +10)
102805                OR (CS-COM-TYP (S1) = 'B')
102805             END-PERFORM
102805             IF (S1 NOT > +10)
102805                AND (CS-COM-TYP (S1) = 'B')
102805                    MOVE CS-COM-TYP (S1) TO WS-AGT-TYPE
102805                    MOVE CS-AGT (S1) TO WS-AGT
102805             ELSE
102805                    MOVE ' ' TO WS-AGT-TYPE
102805                    MOVE ' ' TO WS-AGT
102805             END-IF
102805
102805         WHEN '23'
102805             DISPLAY 'THIS ACCOUNT NOT ON ELCRTT ' CS-ACCOUNT
102805
102805         WHEN OTHER
102805             DISPLAY 'ERROR ON ELCRTT READ ' ELCRTT-STATUS
102805         END-EVALUATE
102805     .
102805 0160-EXIT.
102805     EXIT.
102805

       0200-OUTPUT-ROUTINE.

           PERFORM 0210-RETURN-SORT    THRU 0210-EXIT
               UNTIL END-OF-SORT-FILE

           . 
       0200-EXIT.
           EXIT.


       0210-RETURN-SORT.

           RETURN SORT-WORK                INTO WS-SORT-REC 
               AT END
                   SET END-OF-SORT-FILE    TO TRUE
                   MOVE WS-DEALER-ISSUE-COUNT  TO WS-DTOT-ISSUE-COUNT
                   MOVE WS-DEALER-CANCEL-COUNT TO WS-DTOT-CANCEL-COUNT
102805             MOVE WS-DEALER-CLAIM-COUNT  TO WS-DTOT-CLAIM-COUNT
                   MOVE WS-BANK-ISSUE-COUNT    TO WS-BTOT-ISSUE-COUNT
                   MOVE WS-BANK-CANCEL-COUNT   TO WS-BTOT-CANCEL-COUNT
102805             MOVE WS-BANK-CLAIM-COUNT    TO WS-BTOT-CLAIM-COUNT
                   MOVE WS-DEALER-BANK-FEE-TOTAL 
                                           TO WS-DTOT-NET-BANK-FEES
102805             MOVE WS-DEALER-CLAIM-TOTAL TO WS-DTOT-CLAIM-AMOUNT
                   MOVE WS-BANK-BANK-FEE-TOTAL TO WS-BTOT-NET-BANK-FEES
102805             MOVE WS-BANK-CLAIM-TOTAL TO WS-BTOT-CLAIM-AMOUNT

                   MOVE WS-ALL-ISSUE-COUNT     TO WS-GTOT-ISSUE-COUNT
                   MOVE WS-ALL-CANCEL-COUNT    TO WS-GTOT-CANCEL-COUNT
102805             MOVE WS-ALL-CLAIM-COUNT     TO WS-GTOT-CLAIM-COUNT
                   MOVE WS-ALL-BANK-FEE-TOTAL  TO WS-GTOT-NET-BANK-FEES
102805             MOVE WS-ALL-CLAIM-TOTAL     TO WS-GTOT-CLAIM-AMOUNT

                   MOVE WS-DEALER-TOTAL-LINE   TO PRT
                   WRITE PRT
102805             MOVE WS-DEALER-CLAIM-TOTAL-LINE TO PRT
102805             WRITE PRT
                   MOVE WS-BANK-TOTAL-LINE     TO PRT
                   WRITE PRT
102805             MOVE WS-BANK-CLAIM-TOTAL-LINE   TO PRT
102805             WRITE PRT

                   GO TO 0210-EXIT
           END-RETURN

           ADD +1                          TO WS-RETURN-SORT-COUNT
           IF WS-SORT-BANK-NO = WS-PREV-BANK-NO
               SET SAME-BANK               TO TRUE
           ELSE
               IF NOT-FIRST-TIME   
                   MOVE SPACES             TO WS-PREV-REC-TYPE
                   MOVE SPACE              TO WS-PREV-REFUND-OPT-FLG

                   MOVE WS-DEALER-ISSUE-COUNT  TO WS-DTOT-ISSUE-COUNT
                   MOVE WS-DEALER-CANCEL-COUNT TO WS-DTOT-CANCEL-COUNT
102805             MOVE WS-DEALER-CLAIM-COUNT  TO WS-DTOT-CLAIM-COUNT
                   MOVE WS-BANK-ISSUE-COUNT    TO WS-BTOT-ISSUE-COUNT
                   MOVE WS-BANK-CANCEL-COUNT   TO WS-BTOT-CANCEL-COUNT
102805             MOVE WS-BANK-CLAIM-COUNT    TO WS-BTOT-CLAIM-COUNT
                   MOVE WS-DEALER-BANK-FEE-TOTAL 
                                           TO WS-DTOT-NET-BANK-FEES
102805             MOVE WS-DEALER-CLAIM-TOTAL TO WS-DTOT-CLAIM-AMOUNT
                   MOVE WS-BANK-BANK-FEE-TOTAL TO WS-BTOT-NET-BANK-FEES
102805             MOVE WS-BANK-CLAIM-TOTAL TO WS-BTOT-CLAIM-AMOUNT

                   MOVE WS-DEALER-TOTAL-LINE   TO PRT
                   WRITE PRT
102805             MOVE WS-DEALER-CLAIM-TOTAL-LINE TO PRT
102805             WRITE PRT
                   MOVE WS-BANK-TOTAL-LINE     TO PRT
                   WRITE PRT
102805             MOVE WS-BANK-CLAIM-TOTAL-LINE   TO PRT
102805             WRITE PRT

                   MOVE ZEROS              TO WS-DEALER-ISSUE-COUNT
                   MOVE ZEROS              TO WS-DEALER-CANCEL-COUNT
102805             MOVE ZEROS              TO WS-DEALER-CLAIM-COUNT
                   MOVE ZEROS              TO WS-BANK-ISSUE-COUNT
                   MOVE ZEROS              TO WS-BANK-CANCEL-COUNT
102805             MOVE ZEROS              TO WS-BANK-CLAIM-COUNT
                   MOVE ZEROS              TO WS-DEALER-BANK-FEE-TOTAL
102805             MOVE ZEROS              TO WS-DEALER-CLAIM-TOTAL
                   MOVE ZEROS              TO WS-BANK-BANK-FEE-TOTAL
102805             MOVE ZEROS              TO WS-BANK-CLAIM-TOTAL
                   MOVE +0                 TO WS-PAGE   
                   SET NEW-BANK            TO TRUE
               END-IF            
  
               MOVE WS-SORT-BANK-NO        TO WS-PREV-BANK-NO
               MOVE WS-SORT-CARRIER        TO WS-HD4-1-CARRIER
               MOVE WS-SORT-GROUP          TO WS-HD4-1-GROUP
               MOVE WS-SORT-BANK-NO        TO WS-HD4-1-BANK-NO
 
               MOVE WS-SORT-CARRIER        TO CO-CARRIER
               MOVE WS-SORT-GROUP          TO CO-GROUPING
               MOVE WS-SORT-BANK-NO        TO CO-RESP-NO
               MOVE LOW-VALUES             TO CO-ACCOUNT
               MOVE 'B'                    TO CO-TYPE

110305         READ ERCOMP-FILE
               
               EVALUATE ERCOMP-STATUS
               WHEN '00' 
                   MOVE CO-ACCT-NAME       TO WS-HD4-1-BANK-NAME
                   MOVE CO-ADDR-1          TO WS-HD4-2-BANK-ADDRESS1
                   IF CO-ADDR-2 NOT = SPACES
                       MOVE CO-ADDR-2      TO WS-HD4-3-BANK-ADDRESS2
051810                 MOVE SPACES         TO WS-HD4-4-BANK-ADDRESS3
051810                 STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810                    DELIMITED BY '  ' INTO WS-HD4-4-BANK-ADDRESS3
051810                 END-STRING
                       MOVE CO-ZIP         TO WS-HD4-5-BANK-ADDRESS4
                   ELSE
051810                 MOVE SPACES         TO WS-HD4-3-BANK-ADDRESS2
051810                 STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810                    DELIMITED BY '  ' INTO WS-HD4-3-BANK-ADDRESS2
051810                 END-STRING
                       MOVE CO-ZIP         TO WS-HD4-4-BANK-ADDRESS3
                       MOVE SPACES         TO WS-HD4-5-BANK-ADDRESS4
                   END-IF
           
               WHEN '23'
                   DISPLAY 'THIS BANK NOT ON ERCOMP ' WS-SORT-BANK-NO

               WHEN OTHER
                   DISPLAY 'ERROR ON ERCOMP READ ' ERCOMP-STATUS
               END-EVALUATE

               MOVE +56                    TO WS-LINE-COUNT
           END-IF


           IF WS-SORT-ACCOUNT = WS-PREV-ACCOUNT-NO
               CONTINUE
           ELSE 
               IF NOT-FIRST-TIME
                   IF SAME-BANK
                       MOVE SPACES         TO WS-PREV-REC-TYPE
                       MOVE SPACE          TO WS-PREV-REFUND-OPT-FLG

                       MOVE WS-DEALER-ISSUE-COUNT 
                                           TO WS-DTOT-ISSUE-COUNT
                       MOVE WS-DEALER-CANCEL-COUNT 
                                           TO WS-DTOT-CANCEL-COUNT
102805                 MOVE WS-DEALER-CLAIM-COUNT
102805                                     TO WS-DTOT-CLAIM-COUNT
                       MOVE WS-DEALER-BANK-FEE-TOTAL 
                                           TO WS-DTOT-NET-BANK-FEES
102805                 MOVE WS-DEALER-CLAIM-TOTAL
102805                                     TO WS-DTOT-CLAIM-AMOUNT

                       MOVE WS-DEALER-TOTAL-LINE   TO PRT
                       WRITE PRT
102805                 MOVE WS-DEALER-CLAIM-TOTAL-LINE TO PRT
102805                 WRITE PRT

                       MOVE ZEROS          TO WS-DEALER-ISSUE-COUNT
                       MOVE ZEROS          TO WS-DEALER-CANCEL-COUNT
102805                 MOVE ZEROS          TO WS-DEALER-CLAIM-COUNT
                       MOVE ZEROS          TO WS-DEALER-BANK-FEE-TOTAL
102805                 MOVE ZEROS          TO WS-DEALER-CLAIM-TOTAL
                   END-IF
               ELSE
                   SET NOT-FIRST-TIME      TO TRUE 
               END-IF

               MOVE WS-SORT-ACCOUNT        TO WS-PREV-ACCOUNT-NO
               MOVE WS-SORT-ACCOUNT        TO WS-HD5-ACCT-NO

               MOVE WS-SORT-CARRIER        TO AM-CARRIER
               MOVE WS-SORT-GROUP          TO AM-GROUPING
               MOVE WS-SORT-STATE          TO AM-STATE
               MOVE WS-SORT-ACCOUNT        TO AM-ACCOUNT
               MOVE LOW-VALUES             TO AM-CNTRL-B

110305         START ERACCT-FILE KEY NOT < AM-CONTROL-PRIMARY

               EVALUATE ERACCT-STATUS
               WHEN '00' 
                   MOVE SPACE              TO WS-RECORD-FOUND-SW 
                   MOVE AM-CONTROL-A       TO WS-SAVE-AM-CONTROL-A     
                   PERFORM 0220-GET-DEALER-ACCT-NAME THRU 0220-EXIT
                       UNTIL GOT-MOST-CURRENT 
                   MOVE WS-SAVE-AM-NAME    TO WS-HD5-ACCT-NAME 
           
               WHEN '23'
                   DISPLAY 'THIS DEALER NOT ON ERACCT ' WS-SORT-ACCOUNT

               WHEN OTHER
                   DISPLAY 'ERROR ON ERACCT START ' ERACCT-STATUS
                   DISPLAY 'AM CONTROL PRIMARY ' AM-CONTROL-PRIMARY
                   PERFORM ABEND-PGM       THRU APS-EXIT 
               END-EVALUATE

               MOVE +56                    TO WS-LINE-COUNT
           END-IF

           PERFORM 0300-HDR1-TO-HDR5       THRU 0300-EXIT

102805     IF SRT-CLAIM
102805         ADD WS-SORT-CLM-AMT         TO WS-ALL-CLAIM-TOTAL
102805         ADD WS-SORT-CLM-AMT         TO WS-BANK-CLAIM-TOTAL
102805         ADD WS-SORT-CLM-AMT         TO WS-DEALER-CLAIM-TOTAL
102805         ADD +1                      TO WS-ALL-CLAIM-COUNT
102805         ADD +1                      TO WS-BANK-CLAIM-COUNT
102805         ADD +1                      TO WS-DEALER-CLAIM-COUNT
102805*         MOVE WS-SORT-INSURED-NAME   TO WS-D1C-INSURED-NAME
102805         MOVE SPACES                 TO WS-D1C-INSURED-NAME
102805         MOVE WS-SORT-CERT           TO WS-D1C-CERT   
102805         MOVE WS-SORT-CERT-EFF-DT (4:4) 
102805                                     TO WS-D1C-CERT-EFF-CCYY
102805         MOVE WS-SORT-CERT-EFF-DT (8:2) 
102805                                     TO WS-D1C-CERT-EFF-MM
102805         MOVE WS-SORT-CERT-EFF-DT (10:2) 
102805                                     TO WS-D1C-CERT-EFF-DD
102805         IF WS-SORT-CLM-TYP EQUAL '1' OR '3'
102805             MOVE LIFE-OVERRIDE-L6   TO WS-D1C-CLM-L-A
102805         ELSE
102805             MOVE AH-OVERRIDE-L6     TO WS-D1C-CLM-L-A
102805         END-IF
102805         MOVE 'CLAIM PAID'           TO WS-D1C-CLM-MSG
102805         MOVE WS-SORT-CLM-AMT        TO WS-D1C-CLM-AMT
102805     ELSE
           IF REF-ISSUE OR NON-REF-ISSUE
               ADD WS-SORT-BANK-FEE        TO WS-ALL-BANK-FEE-TOTAL
               ADD WS-SORT-BANK-FEE        TO WS-BANK-BANK-FEE-TOTAL
               ADD WS-SORT-BANK-FEE        TO WS-DEALER-BANK-FEE-TOTAL
               ADD +1                      TO WS-ALL-ISSUE-COUNT
               ADD +1                      TO WS-BANK-ISSUE-COUNT
               ADD +1                      TO WS-DEALER-ISSUE-COUNT
  
               MOVE WS-SORT-INSURED-NAME   TO WS-D1A-INSURED-NAME
               MOVE WS-SORT-CERT           TO WS-D1A-CERT   
               MOVE WS-SORT-CERT-EFF-DT (4:4) 
                                           TO WS-D1A-CERT-EFF-CCYY
               MOVE WS-SORT-CERT-EFF-DT (8:2) 
                                           TO WS-D1A-CERT-EFF-MM
               MOVE WS-SORT-CERT-EFF-DT (10:2) 
                                           TO WS-D1A-CERT-EFF-DD
               MOVE WS-SORT-AGE            TO WS-D1A-AGE
               MOVE WS-SORT-AH-TERM        TO WS-D1A-AH-TERM
               MOVE WS-SORT-AH-TYPE        TO WS-D1A-AH-TYPE
               MOVE WS-SORT-PRM-AMT        TO WS-D1A-PRM-AMT
               MOVE WS-SORT-BANK-FEE       TO WS-D1A-BANK-FEE
           ELSE
               SUBTRACT WS-SORT-BANK-FEE   FROM WS-ALL-BANK-FEE-TOTAL
               SUBTRACT WS-SORT-BANK-FEE   FROM WS-BANK-BANK-FEE-TOTAL
               SUBTRACT WS-SORT-BANK-FEE   FROM WS-DEALER-BANK-FEE-TOTAL
               ADD +1                      TO WS-ALL-CANCEL-COUNT
               ADD +1                      TO WS-BANK-CANCEL-COUNT
               ADD +1                      TO WS-DEALER-CANCEL-COUNT

               MOVE WS-SORT-INSURED-NAME   TO WS-D1B-INSURED-NAME
               MOVE WS-SORT-CERT           TO WS-D1B-CERT    
               MOVE WS-SORT-CERT-EFF-DT (4:4) 
                                           TO WS-D1B-CERT-EFF-CCYY
               MOVE WS-SORT-CERT-EFF-DT (8:2) 
                                           TO WS-D1B-CERT-EFF-MM
               MOVE WS-SORT-CERT-EFF-DT (10:2) 
                                           TO WS-D1B-CERT-EFF-DD
               MOVE WS-SORT-AH-CAN-DT (4:4) 
                                           TO WS-D1B-CERT-CAN-CCYY
               MOVE WS-SORT-AH-CAN-DT (8:2) 
                                           TO WS-D1B-CERT-CAN-MM
               MOVE WS-SORT-AH-CAN-DT (10:2) 
                                           TO WS-D1B-CERT-CAN-DD
               MOVE WS-SORT-AH-TYPE        TO WS-D1B-AH-TYPE
               MOVE WS-SORT-AH-REFUND      TO WS-D1B-REFUND-AMT
               MOVE WS-SORT-BANK-FEE       TO WS-D1B-REFUND-BKFEE
               MOVE WS-SORT-PRM-AMT        TO WS-D1B-PRM-AMT
           END-IF
102805     END-IF           
 
           IF WS-SORT-REC-TYPE = WS-PREV-REC-TYPE
              CONTINUE
           ELSE
               MOVE WS-SORT-REC-TYPE       TO WS-PREV-REC-TYPE
      *        MOVE WS-SORT-REFUND-OPT-FLG TO WS-PREV-REFUND-OPT-FLG
               PERFORM 0310-SECTION-HDR    THRU 0310-EXIT
           END-IF

102805     IF SRT-CLAIM
102805         MOVE WS-DETAIL1C            TO PRT
102805     ELSE
           IF REF-ISSUE OR NON-REF-ISSUE
               MOVE WS-DETAIL1A            TO PRT
           ELSE
               MOVE WS-DETAIL1B            TO PRT
           END-IF 
102805     END-IF           

           PERFORM 0320-PRINT-DETAIL       THRU 0320-EXIT

           . 
       0210-EXIT.
           EXIT.

       0220-GET-DEALER-ACCT-NAME.

110305     READ ERACCT-FILE NEXT

           EVALUATE ERACCT-STATUS
           WHEN '00' 
               IF AM-CONTROL-A = WS-SAVE-AM-CONTROL-A
                   MOVE AM-NAME         TO WS-SAVE-AM-NAME    
               ELSE
                   SET GOT-MOST-CURRENT TO TRUE
               END-IF    
         
           WHEN '10'
               DISPLAY 'END OF ERACCT FILE'
               SET GOT-MOST-CURRENT     TO TRUE
               GO TO 0220-EXIT

           WHEN OTHER
               DISPLAY 'ERROR ON ERACCT READ NEXT ' ERACCT-STATUS
               DISPLAY 'AM CONTROL PRIMARY ' AM-CONTROL-PRIMARY 
               PERFORM ABEND-PGM        THRU APS-EXIT
           END-EVALUATE

 
           . 
       0220-EXIT.
           EXIT.


       0300-HDR1-TO-HDR5.

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
               MOVE +0                     TO WS-LINE-COUNT
               ADD +1                      TO WS-PAGE 
               MOVE WS-PAGE                TO WS-HD3-PAGE
               WRITE PRT                   FROM WS-HD1 
               WRITE PRT                   FROM WS-HD2
               WRITE PRT                   FROM WS-HD3
               WRITE PRT                   FROM WS-HD4-1
               WRITE PRT                   FROM WS-HD4-2
               WRITE PRT                   FROM WS-HD4-3
               WRITE PRT                   FROM WS-HD4-4
               WRITE PRT                   FROM WS-HD4-5
               WRITE PRT                   FROM WS-HD5

               ADD +12                     TO WS-LINE-COUNT
           END-IF 

           . 
       0300-EXIT.
           EXIT.

       0310-SECTION-HDR.

      *            88  REF-ISSUE                       VALUE '01'.
      *            88  REF-CANCEL                      VALUE '02'.
      *            88  NON-REF-ISSUE                   VALUE '03'.
      *            88  NON-REF-CANCEL                  VALUE '04'.


           EVALUATE TRUE
           WHEN NON-REF-ISSUE
               MOVE SPACES                 TO PRT
               WRITE PRT
               WRITE PRT                   FROM WS-HDR-DASH-LINE
               WRITE PRT                   FROM WS-HD6A
               WRITE PRT                   FROM WS-HDR-DASH-LINE 
               WRITE PRT                   FROM WS-HD7A
               WRITE PRT                   FROM WS-HD8A
               MOVE SPACES                 TO PRT
               WRITE PRT

           WHEN REF-CANCEL OR NON-REF-CANCEL
               MOVE SPACES                 TO PRT
               WRITE PRT
               WRITE PRT                   FROM WS-HDR-DASH-LINE
               WRITE PRT                   FROM WS-HD6B
               WRITE PRT                   FROM WS-HDR-DASH-LINE 
               WRITE PRT                   FROM WS-HD7B
               WRITE PRT                   FROM WS-HD8B
               MOVE SPACES                 TO PRT
               WRITE PRT

           WHEN REF-ISSUE
               MOVE SPACES                 TO PRT
               WRITE PRT
               WRITE PRT                   FROM WS-HDR-DASH-LINE
               WRITE PRT                   FROM WS-HD6C
               WRITE PRT                   FROM WS-HDR-DASH-LINE 
               WRITE PRT                   FROM WS-HD7A
               WRITE PRT                   FROM WS-HD8A
               MOVE SPACES                 TO PRT
               WRITE PRT
102805
102805     WHEN SRT-CLAIM
102805         MOVE SPACES                 TO PRT
102805         WRITE PRT
102805         WRITE PRT                   FROM WS-HDR-DASH-LINE
102805         WRITE PRT                   FROM WS-HD6D
102805         WRITE PRT                   FROM WS-HDR-DASH-LINE 
102805         WRITE PRT                   FROM WS-HD7A
102805         WRITE PRT                   FROM WS-HD8C
102805         MOVE SPACES                 TO PRT
102805         WRITE PRT
           END-EVALUATE

           ADD +8                          TO WS-LINE-COUNT

           . 
       0310-EXIT.
           EXIT.


       0320-PRINT-DETAIL.

           WRITE PRT
           MOVE SPACES                     TO PRT
           WRITE PRT
           ADD +2                          TO WS-LINE-COUNT

           . 
       0320-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
110305                ERCOMP-FILE
110305                ERACCT-FILE
102805                ELCRTT-FILE
               OUTPUT PRT-OUT

           IF ERCOMP-STATUS = '00' OR '97'   
               CONTINUE        
           ELSE               
               DISPLAY 'OPEN ERROR ' ERCOMP-STATUS ' ON ERCOMP' 
               DISPLAY '*** JOB ABORTED ***'
               PERFORM ABEND-PGM     THRU APS-EXIT
           END-IF             

           IF ERACCT-STATUS = '00' OR '97'   
               CONTINUE        
           ELSE               
               DISPLAY 'OPEN ERROR ' ERACCT-STATUS ' ON ERACCT' 
               DISPLAY '*** JOB ABORTED ***'
               PERFORM ABEND-PGM     THRU APS-EXIT
           END-IF             
102805
102805     IF ELCRTT-STATUS = '00' OR '97'
102805         CONTINUE
102805     ELSE
102805         DISPLAY 'OPEN ERROR ' ELCRTT-STATUS ' ON ELCRTT'
102805         DISPLAY '*** JOB ABORTED ***'
102805         PERFORM ABEND-PGM     THRU APS-EXIT
102805     END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN
110305           ERCOMP-FILE
110305           ERACCT-FILE
102805           ELCRTT-FILE
                 PRT-OUT

           IF ERCOMP-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON CLOSE - ERCOMP ' ERCOMP-STATUS
              PERFORM ABEND-PGM      THRU APS-EXIT  
           END-IF


           IF ERACCT-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON CLOSE - ERACCT ' ERACCT-STATUS
              PERFORM ABEND-PGM      THRU APS-EXIT  
           END-IF
102805
102805     IF ELCRTT-STATUS = '00' OR '97'
102805        CONTINUE
102805     ELSE
102805        DISPLAY ' ERROR ON CLOSE - ELCRTT ' ELCRTT-STATUS
102805        PERFORM ABEND-PGM      THRU APS-EXIT
102805     END-IF

           .

       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE COMPANY-NAME           TO WS-HD2-CO

           ACCEPT WS-ACCEPT-DATE       FROM DATE
           MOVE WS-AD-MM               TO WS-CD-MM
           MOVE WS-AD-DD               TO WS-CD-DD
           MOVE WS-AD-YY               TO WS-CD-YY
           MOVE WS-CURRENT-DATE        TO WS-HD2-RUN-DT

           MOVE ALPH-DATE              TO WS-HD3-ALPHA-DT
     
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           
           MOVE ZEROS                  TO WS-DTOT-NET-BANK-FEES
           MOVE ZEROS                  TO WS-BTOT-NET-BANK-FEES
           MOVE ZEROS                  TO WS-GTOT-NET-BANK-FEES
102805     MOVE ZEROS                  TO WS-DTOT-CLAIM-AMOUNT
102805     MOVE ZEROS                  TO WS-BTOT-CLAIM-AMOUNT
102805     MOVE ZEROS                  TO WS-GTOT-CLAIM-AMOUNT

           .
       0600-EXIT.
           EXIT.

102805
102805 8510-DATE-CONVERSION.
102805
102805     CALL 'ELDATCX' USING DATE-CONVERSION-DATA
102805
102805     .
102805
102805 8590-EXIT.
102805     EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.
