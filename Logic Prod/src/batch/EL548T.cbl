       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL548.
       AUTHOR.     CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
      *REMARKS.
      
102902******************************************************************
102902*                   C H A N G E   L O G
102902*
102902* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102902*-----------------------------------------------------------------
102902*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102902* EFFECTIVE    NUMBER
102902*-----------------------------------------------------------------
102902* 102902                   PEMA  CORRECT EXPIRATION DATE   
102902*                                CONVERSION
022703* 022703                   PEMA  ADD CURRENT DATE RANGE
022703*                                INDICATOR
022703*                                ALSO, CONVERT LCASE TO UCASE
053003* 053003                   PEMA  ADD CHECK FOR NON NUMERIC
053003*                                COMM. TABLE CODE
082603* 082603                   PEMA  ADD DATE FILE PROCESSING 
052804* 052804                   SMVA  FIX CUR DTE RNG IND * ON LAST ACCT
101305* 101305   2005072100004   PEMA  DON'T PASS INVALID COMM LEVELS
102902******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT        ASSIGN TO ERACCT
                                ORGANIZATION IS INDEXED
                                ACCESS IS DYNAMIC
                                RECORD KEY IS ERACCT-CONTROL-PRIMARY
                                FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERCOMP        ASSIGN TO ERCOMP
                                ORGANIZATION IS INDEXED
                                ACCESS IS DYNAMIC
                                RECORD KEY IS CO-CONTROL-PRIMARY
                                FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT ACCT-OUT      ASSIGN TO ACCTOUT
               ORGANIZATION IS LINE SEQUENTIAL.

082603     SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

       01  ERACCT-IN-RECORD.
           05  FILLER                  PIC XX.
           05  ERACCT-CONTROL-PRIMARY.
               10  ERACCT-COMPANY-CD   PIC X.
               10  ERACCT-ACCT-KEY     PIC X(19).
               10  FILLER              PIC X(6).
           05  FILLER                  PIC X(1972).


       FD  ERCOMP.

           COPY ERCCOMP.
      /

       FD  ACCT-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

022703 01  ACCT-OUT-REC                PIC X(1533).
022703 01  ACCT-HEAD-REC               PIC X(1689).

082603 FD  DISK-DATE                   COPY ELCDTEFD.



           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   EL548 WORKING STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
022703     88  END-OF-ERACCT             VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  ACT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  ACT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
      /
022703     COPY ERCACCT.
       01  ACCT-DETAIL-RECORD.
           12  ACCT-CARRIER            PIC X.
           12  ACCT-TAB1               PIC X.
           12  ACCT-GROUPING           PIC X(6).
           12  ACCT-TAB2               PIC X.
           12  ACCT-STATE              PIC XX.
           12  ACCT-TAB3               PIC X.
           12  ACCT-ACCOUNT            PIC X(10).
           12  ACCT-TAB4               PIC X.
           12  ACCT-EXP-DATE           PIC X(10).
           12  ACCT-TAB5               PIC X.
           12  ACCT-EFF-DATE           PIC X(10).
           12  ACCT-TAB6               PIC X.
           12  ACCT-REPORT-CODE-1      PIC X(10).
           12  ACCT-TAB7               PIC X.
           12  ACCT-REPORT-CODE-2      PIC X(10).
           12  ACCT-TAB8               PIC X.
           12  ACCT-CITY-CODE          PIC X(4).
           12  ACCT-TAB9               PIC X.
           12  ACCT-COUNTY-PARISH      PIC X(6).
           12  ACCT-TAB10              PIC X.
           12  ACCT-NAME               PIC X(30).
           12  ACCT-TAB11              PIC X.
           12  ACCT-PERSON             PIC X(30).
           12  ACCT-TAB12              PIC X.
           12  ACCT-ADDRS              PIC X(30).
           12  ACCT-TAB13              PIC X.
           12  ACCT-CITY               PIC X(30).
           12  ACCT-TAB14              PIC X.
           12  ACCT-ZIP                PIC X(9).
           12  ACCT-TAB15              PIC X.
           12  ACCT-TEL-NO             PIC X(10).
           12  ACCT-TAB16              PIC X.
           12  ACCT-COMM-STRUCTURE.
               16  ACCT-DEFN-1.
                   20  ACCT-AGT-COMMS     OCCURS 8 TIMES.
                       24  ACCT-AGT.
                           28  ACCT-AGT-PREFIX   PIC X(4).
                           28  ACCT-AGT-PRIME    PIC X(6).
                       24  ACCT-TAB17            PIC X.
                       24  ACCT-COM-TYP          PIC X.
                       24  ACCT-TAB18            PIC X.
                       24  ACCT-L-COM            PIC -.99999.
                       24  ACCT-TAB19            PIC X.
                       24  ACCT-J-COM            PIC -.99999.
                       24  ACCT-TAB20            PIC X.
                       24  ACCT-A-COM            PIC -.99999.
                       24  ACCT-TAB21            PIC X.
                       24  ACCT-RECALC-LV-INDIC  PIC X.
                       24  ACCT-TAB22            PIC X.
                       24  ACCT-RETRO-LV-INDIC   PIC X.
                       24  ACCT-TAB23            PIC X.
                       24  ACCT-GL-CODES         PIC X.
                       24  ACCT-TAB24            PIC X.
                       24  ACCT-COMM-CHARGEBACK  PIC XX.
                       24  ACCT-TAB25            PIC X.
               16  ACCT-DEFN-2 REDEFINES   ACCT-DEFN-1.
                   20  ACCT-COM-TBLS      OCCURS 8 TIMES.
                       24  FILLER                PIC X(13).
                       24  ACCT-L-COMA           PIC X(7).
                       24  FILLER                PIC X.
                       24  ACCT-J-COMA           PIC X(7).
                       24  FILLER                PIC X.
                       24  ACCT-A-COMA           PIC X(7).
                       24  FILLER                PIC X.
                       24  FILLER                PIC X(9).

           12  ACCT-CSR-CODE           PIC X(4).
           12  ACCT-TAB27              PIC X.
           12  ACCT-GPCD               PIC XX.
           12  ACCT-TAB28              PIC X.
           12  ACCT-IG                 PIC X.
           12  ACCT-TAB29              PIC X.
           12  ACCT-STATUS             PIC X.
           12  ACCT-TAB30              PIC X.
           12  ACCT-REMIT-TO           PIC XX.
           12  ACCT-TAB31              PIC X.
           12  ACCT-ID-NO              PIC X(11).
           12  ACCT-TAB32              PIC X.

           12  ACCT-CAL-TABLE                    PIC XX.
           12  ACCT-TAB33                        PIC X.
           12  ACCT-USER-FIELDS.
               16  ACCT-FLD-1                    PIC XX.
               16  ACCT-TAB42                    PIC X.
               16  ACCT-FLD-2                    PIC XX.
               16  ACCT-TAB43                    PIC X.
               16  ACCT-FLD-3                    PIC XX.
               16  ACCT-TAB44                    PIC X.
               16  ACCT-FLD-4                    PIC XX.
               16  ACCT-TAB45                    PIC X.
               16  ACCT-FLD-5                    PIC XX.
               16  ACCT-TAB46                    PIC X.

           12  ACCT-1ST-PROD-DATE                PIC X(10).
           12  ACCT-TAB47                        PIC X.
           12  ACCT-ANNIVERSARY-DATE             PIC X(10).
           12  ACCT-TAB48                        PIC X.
           12  ACCT-CERTS-PURGED-DATE            PIC X(10).
           12  ACCT-TAB49                        PIC X.
           12  ACCT-HI-CERT-DATE                 PIC X(10).
           12  ACCT-TAB50                        PIC X.
           12  ACCT-LO-CERT-DATE                 PIC X(10).
           12  ACCT-TAB51                        PIC X.
           12  ACCT-ENTRY-DATE                   PIC X(10).
           12  ACCT-TAB52                        PIC X.
           12  ACCT-INACTIVE-DATE                PIC X(10).
           12  ACCT-TAB53                        PIC X.
           12  ACCT-AH-ONLY-INDICATOR            PIC X.
           12  ACCT-TAB54                        PIC X.
           12  ACCT-EDIT-LOAN-OFC                PIC X(01).
           12  ACCT-TAB55                        PIC X.
           12  ACCT-OVER-SHORT.
               16  ACCT-OVR-SHT-AMT              PIC -999.99.
               16  ACCT-TAB56                    PIC X.
               16  ACCT-OVR-SHT-PCT              PIC -9.9999.
           12  ACCT-TAB57                        PIC X.
           12  ACCT-RECALC-COMM                  PIC X.
           12  ACCT-TAB58                        PIC X.
           12  ACCT-RECALC-REIN                  PIC X.
           12  ACCT-TAB59                        PIC X.
           12  ACCT-REI-TABLE                    PIC XXX.
           12  ACCT-TAB60                        PIC X.
           12  ACCT-REI-LF-TAX                   PIC -9.9999.
           12  ACCT-TAB68                        PIC X.
           12  ACCT-REI-PR-PCT                   PIC -9.9999.
           12  ACCT-TAB72                        PIC X.
           12  ACCT-REI-78-PCT                   PIC -9.9999.
           12  ACCT-TAB73                        PIC X.
           12  ACCT-REI-AH-TAX                   PIC -9.9999.
           12  ACCT-TAB74                        PIC X.
           12  ACCT-STD-AH-TYPE                  PIC XX.
           12  ACCT-TAB79                        PIC X.
           12  ACCT-EARN-METHODS.
               16  ACCT-EARN-METHOD-R            PIC X.
               16  ACCT-TAB80                    PIC X.
               16  ACCT-EARN-METHOD-L            PIC X.
               16  ACCT-TAB81                    PIC X.
               16  ACCT-EARN-METHOD-A            PIC X.
               16  ACCT-TAB82                    PIC X.

           12  ACCT-TOL-PREM                     PIC -999.99.
           12  ACCT-TAB83                        PIC X.
           12  ACCT-TOL-REF                      PIC -999.99.
           12  ACCT-TAB84                        PIC X.
           12  ACCT-TOL-CLM                      PIC -999.99.
           12  ACCT-TAB85                        PIC X.

           12  ACCT-RET-Y-N                      PIC X.
           12  ACCT-TAB86                        PIC X.
           12  ACCT-RET-P-E                      PIC X.
           12  ACCT-TAB87                        PIC X.
           12  ACCT-LF-RET                       PIC -9.9999.
           12  ACCT-TAB88                        PIC X.
           12  ACCT-AH-RET                       PIC -9.9999.
           12  ACCT-TAB89                        PIC X.
           12  ACCT-RET-GRP                      PIC X(6).
           12  ACCT-TAB90                        PIC X.
           12  ACCT-RETRO-EARNINGS.
               16  ACCT-RET-EARN-R               PIC X.
               16  ACCT-TAB91                    PIC X.
               16  ACCT-RET-EARN-L               PIC X.
               16  ACCT-TAB92                    PIC X.
               16  ACCT-RET-EARN-A               PIC X.
               16  ACCT-TAB93                    PIC X.
           12  ACCT-RET-ST-TAX-USE               PIC X.
           12  ACCT-TAB94                        PIC X.

           12  ACCT-USER-SELECT-OPTIONS.
               16  ACCT-USER-SELECT-1            PIC X(10).
               16  ACCT-TAB95                    PIC X.
               16  ACCT-USER-SELECT-2            PIC X(10).
               16  ACCT-TAB96                    PIC X.
               16  ACCT-USER-SELECT-3            PIC X(10).
               16  ACCT-TAB97                    PIC X.
               16  ACCT-USER-SELECT-4            PIC X(10).
               16  ACCT-TAB98                    PIC X.
               16  ACCT-USER-SELECT-5            PIC X(10).
               16  ACCT-TAB99                    PIC X.

           12  ACCT-LF-RPT021-EXP-PCT            PIC -999.9999.
           12  ACCT-TAB100                       PIC X.

           12  ACCT-AH-RPT021-EXP-PCT            PIC -999.9999.
           12  ACCT-TAB101                       PIC X.

           12  ACCT-RPT045A-SWITCH               PIC X.
           12  ACCT-TAB102                       PIC X.

           12  ACCT-INSURANCE-LIMITS.
               16  ACCT-MAX-MON-BEN              PIC -9(7).
               16  ACCT-TAB103                   PIC X.
               16  ACCT-MAX-TOT-BEN              PIC -9(7).
               16  ACCT-TAB104                   PIC X.

           12  ACCT-DISMBR-COVERAGE-SW           PIC X.
           12  ACCT-TAB105                       PIC X.

           12  ACCT-CANCEL-FEE                   PIC -999.99.
           12  ACCT-TAB106                       PIC X.

           12  ACCT-TOL-REF-PCT                  PIC -9.9(4).
           12  ACCT-TAB107                       PIC X.


           12  ACCT-3RD-PARTY-NOTIF-LEVEL        PIC 99.
           12  ACCT-TAB108                       PIC X.
           12  ACCT-NOTIFICATION-TYPES.
               16  ACCT-NOTIF-OF-LETTERS         PIC X.
               16  ACCT-TAB109                   PIC X.
               16  ACCT-NOTIF-OF-PAYMENTS        PIC X.
               16  ACCT-TAB110                   PIC X.
               16  ACCT-NOTIF-OF-REPORTS         PIC X.
               16  ACCT-TAB111                   PIC X.
               16  ACCT-NOTIF-OF-STATUS          PIC X.
               16  ACCT-TAB112                   PIC X.

           12  ACCT-BENEFIT-TABLE-USAGE          PIC X.
           12  ACCT-TAB113                       PIC X.

           12  ACCT-BENEFIT-CONTROLS.
               16  ACCT-ALLOWABLE-BENEFITS OCCURS 10  TIMES.
                   20  ACCT-BENEFIT-CODE         PIC XX.
                   20  ACCT-TAB114               PIC X.
                   20  ACCT-BENEFIT-TYPE         PIC X.
                   20  ACCT-TAB115               PIC X.
                   20  ACCT-BENEFIT-REVISION     PIC XXX.
                   20  ACCT-TAB116               PIC X.
                   20  ACCT-BENEFIT-REM-TERM     PIC X.
                   20  ACCT-TAB117               PIC X.
                   20  ACCT-BENEFIT-RETRO-Y-N    PIC X.
                   20  ACCT-TAB118               PIC X.



           12  ACCT-CONTROL-NAME             PIC X(30).
           12  ACCT-TAB120                       PIC X.

           12  ACCT-RETRO-ADDITIONAL-DATA.
               16  ACCT-RETRO-QUALIFY-LIMIT      PIC -9(7).
               16  ACCT-TAB121                   PIC X.
               16  ACCT-RETRO-PREM-P-E           PIC X.
               16  ACCT-TAB122                   PIC X.
               16  ACCT-RETRO-CLMS-P-I           PIC X.
               16  ACCT-TAB123                   PIC X.
               16  ACCT-RETRO-RET-BRACKET-LF.
                   20  ACCT-RETRO-RET-METHOD-LF  PIC X.
                   20  ACCT-TAB124               PIC X.
                   20  ACCT-RETRO-RET-BASIS-LF   PIC X.
                   20  ACCT-TAB125               PIC X.
                   20  ACCT-RETRO-BRACKETS-LF OCCURS 3 TIMES.
                       24  ACCT-RETRO-RET-PCT-LF   PIC -9.9999.
                       24  ACCT-TAB126             PIC X.
                       24  ACCT-RETRO-RET-THRU-LF  PIC -9(7).
                       24  ACCT-TAB127             PIC X.
               16  ACCT-RETRO-RET-BRACKET-AH.
                   20  ACCT-RETRO-RET-METHOD-AH  PIC X.
                   20  ACCT-TAB128               PIC X.
                   20  ACCT-RETRO-RET-BASIS-AH   PIC X.
                   20  ACCT-TAB129               PIC X.
                   20  ACCT-RETRO-BRACKETS-AH OCCURS 3 TIMES.
                       24  ACCT-RETRO-RET-PCT-AH PIC -9.9999.
                       24  ACCT-TAB130           PIC X.
                       24  ACCT-RETRO-RET-THRU-AH PIC -9(7).
                       24  ACCT-TAB131           PIC X.

           12  ACCT-COMMENTS OCCURS 5 TIMES.
               16  ACCT-COMMENT-LINE         PIC X(50).
               16  ACCT-TAB132               PIC X.
           12  ACCT-REI-GROUP-A                  PIC X(6).
           12  ACCT-TAB133             PIC X.
           12  ACCT-REI-GROUP-B        PIC X(6).
           12  ACCT-TAB134             PIC X.
           12  ACCT-FAX-NO             PIC X(10).
           12  ACCT-TAB135             PIC X.
           12  ACCT-CURRENT-DTE-RANGE  PIC X.
      /
       01  ACCT-HEADER-RECORD.
           12  HEAD-CARRIER            PIC X(7)  VALUE 'CARRIER'.
           12  HEAD-TAB1               PIC X.
           12  HEAD-GROUPING           PIC X(5)  VALUE 'GROUP'.
           12  HEAD-TAB2               PIC X.
           12  HEAD-STATE              PIC X(5)  VALUE 'STATE'.
           12  HEAD-TAB3               PIC X.
           12  HEAD-ACCOUNT            PIC X(4)  VALUE 'ACCT'.
           12  HEAD-TAB4               PIC X.
           12  HEAD-EXP-DATE           PIC X(6)  VALUE 'EXP DT'.
           12  HEAD-TAB5               PIC X.
           12  HEAD-EFF-DATE           PIC X(10) VALUE 'EFF DT'.
           12  HEAD-TAB6               PIC X.
           12  HEAD-REPORT-CODE-1      PIC X(7)  VALUE 'RPT CD1'.
           12  HEAD-TAB7               PIC X.
           12  HEAD-REPORT-CODE-2      PIC X(7)  VALUE 'RPT CD2'.
           12  HEAD-TAB8               PIC X.
           12  HEAD-CITY-CODE          PIC X(7)  VALUE 'CITY CD'.
           12  HEAD-TAB9               PIC X.
           12  HEAD-COUNTY-PARISH      PIC X(8)  VALUE 'CNTY/PAR'.
           12  HEAD-TAB10              PIC X.
           12  HEAD-NAME               PIC X(9)  VALUE 'ACCT NAME'.
           12  HEAD-TAB11              PIC X.
           12  HEAD-PERSON             PIC X(7)  VALUE 'CONTACT'.
           12  HEAD-TAB12              PIC X.
           12  HEAD-ADDRS              PIC X(7)  VALUE 'ADDRESS'.
           12  HEAD-TAB13              PIC X.
           12  HEAD-CITY               PIC X(8)  VALUE 'CITY, ST'.
           12  HEAD-TAB14              PIC X.
           12  HEAD-ZIP                PIC XXX   VALUE 'ZIP'.
           12  HEAD-TAB15              PIC X.
           12  HEAD-TEL-NO             PIC X(8)  VALUE 'PHONE NO'.
           12  HEAD-TAB16              PIC X.
           12  HEAD-AGT-COMMS     OCCURS 8 TIMES.
               16  HEAD-AGT              PIC X(5)   VALUE 'AGT1 '.
               16  HEAD-TAB17            PIC X.
               16  HEAD-COM-TYP          PIC X(5)   VALUE 'TYP1 '.
               16  HEAD-TAB18            PIC X.
               16  HEAD-L-COM            PIC X(7)   VALUE 'L COM1 '.
               16  HEAD-TAB19            PIC X.
               16  HEAD-J-COM            PIC X(7)   VALUE 'J COM1 '.
               16  HEAD-TAB20            PIC X.
               16  HEAD-A-COM            PIC X(7)   VALUE 'A COM1 '.
               16  HEAD-TAB21            PIC X.
               16  HEAD-RECALC-LV-INDIC  PIC X(6)   VALUE 'RCAL1 '.
               16  HEAD-TAB22            PIC X.
               16  HEAD-RETRO-LV-INDIC   PIC X(5)   VALUE 'RET1 '.
               16  HEAD-TAB23            PIC X.
               16  HEAD-GL-CODES         PIC X(4)   VALUE 'GL1 '.
               16  HEAD-TAB24            PIC X.
               16  HEAD-COMM-CHARGEBACK  PIC X(6)   VALUE 'CHBK1 '.
               16  HEAD-TAB25            PIC X.

           12  HEAD-CSR-CODE           PIC XXX     VALUE 'CSR'.
           12  HEAD-TAB27              PIC X.
           12  HEAD-GPCD               PIC X(4)    VALUE 'GPCD'.
           12  HEAD-TAB28              PIC X.
           12  HEAD-IG                 PIC XXX     VALUE 'I G'.
           12  HEAD-TAB29              PIC X.
           12  HEAD-STATUS             PIC X(6)    VALUE 'STATUS'.
           12  HEAD-TAB30              PIC X.
           12  HEAD-REMIT-TO           PIC X(5)    VALUE 'REMIT'.
           12  HEAD-TAB31              PIC X.
           12  HEAD-ID-NO              PIC X(7)    VALUE 'ACCT ID'.
           12  HEAD-TAB32              PIC X.

           12  HEAD-CAL-TABLE          PIC X(5)   VALUE 'CLASS'.
           12  HEAD-TAB33              PIC X.
           12  HEAD-USER-FIELDS.
               16  HEAD-FLD-1          PIC XX     VALUE 'U1'.
               16  HEAD-TAB42          PIC X.
               16  HEAD-FLD-2          PIC XX     VALUE 'U2'.
               16  HEAD-TAB43          PIC X.
               16  HEAD-FLD-3          PIC XX     VALUE 'U3'.
               16  HEAD-TAB44          PIC X.
               16  HEAD-FLD-4          PIC XX     VALUE 'U4'.
               16  HEAD-TAB45          PIC X.
               16  HEAD-FLD-5          PIC XX     VALUE 'U5'.
               16  HEAD-TAB46          PIC X.

           12  HEAD-1ST-PROD-DATE      PIC X(8)   VALUE '1ST PROD'.
           12  HEAD-TAB47              PIC X.
           12  HEAD-ANNIVERSARY-DATE   PIC X(8)   VALUE 'ANV DATE'.
           12  HEAD-TAB48              PIC X.
           12  HEAD-CERTS-PURGED-DATE  PIC X(8)   VALUE 'CR PG DT'.
           12  HEAD-TAB49              PIC X.
           12  HEAD-HI-CERT-DATE       PIC X(5)   VALUE 'HI DT'.
           12  HEAD-TAB50              PIC X.
           12  HEAD-LO-CERT-DATE       PIC X(5)   VALUE 'LO DT'.
           12  HEAD-TAB51              PIC X.
           12  HEAD-ENTRY-DATE         PIC X(6)   VALUE 'END DT'.
           12  HEAD-TAB52              PIC X.
           12  HEAD-INACTIVE-DATE      PIC X(8)   VALUE 'INACT DT'.
           12  HEAD-TAB53              PIC X.
           12  HEAD-AH-ONLY-INDICATOR  PIC X(7)   VALUE 'AH ONLY'.
           12  HEAD-TAB54              PIC X.
           12  HEAD-EDIT-LOAN-OFC      PIC X(8)   VALUE 'LOAN OFC'.
           12  HEAD-TAB55              PIC X.
           12  HEAD-OVR-SHT-AMT        PIC X(6)   VALUE 'OS AMT'.
           12  HEAD-TAB56              PIC X.
           12  HEAD-OVR-SHT-PCT        PIC X(6)   VALUE 'OS PCT'.
           12  HEAD-TAB57              PIC X.
           12  HEAD-RECALC-COMM        PIC X(9)   VALUE 'COMM RCAL'.
           12  HEAD-TAB58              PIC X.
           12  HEAD-RECALC-REIN        PIC X(9)   VALUE 'REIN RCAL'.
           12  HEAD-TAB59              PIC X.
           12  HEAD-REI-TABLE          PIC X(9)   VALUE 'REI TABLE'.
           12  HEAD-TAB60              PIC X.
           12  HEAD-REI-LF-TAX         PIC X(6)   VALUE 'LF TAX'.
           12  HEAD-TAB68              PIC X.
           12  HEAD-REI-PR-PCT         PIC X(6)   VALUE 'PR PCT'.
           12  HEAD-TAB72              PIC X.
           12  HEAD-REI-78-PCT         PIC X(6)   VALUE '78 PCT'.
           12  HEAD-TAB73              PIC X.
           12  HEAD-REI-AH-TAX         PIC X(6)   VALUE 'AH TAX'.
           12  HEAD-TAB74              PIC X.

           12  HEAD-STD-AH-TYPE        PIC X(6)   VALUE 'STD AH'.
           12  HEAD-TAB79              PIC X.
           12  HEAD-EARN-METHOD-R      PIC X(5)  VALUE 'EARNR'.
           12  HEAD-TAB80              PIC X.
           12  HEAD-EARN-METHOD-L      PIC X(5)  VALUE 'EARNL'.
           12  HEAD-TAB81              PIC X.
           12  HEAD-EARN-METHOD-A      PIC X(5)  VALUE 'EARNA'.
           12  HEAD-TAB82              PIC X.

           12  HEAD-TOL-PREM           PIC X(8)  VALUE 'PREM TOL'.
           12  HEAD-TAB83              PIC X.
           12  HEAD-TOL-REF            PIC X(7)  VALUE 'REF TOL'.
           12  HEAD-TAB84              PIC X.
           12  HEAD-TOL-CLM            PIC X(7)  VALUE 'CLM TOL'.
           12  HEAD-TAB85              PIC X.

           12  HEAD-RET-Y-N            PIC X(5)  VALUE 'RETYN'.
           12  HEAD-TAB86              PIC X.
           12  HEAD-RET-P-E            PIC X(5)  VALUE 'RETPE'.
           12  HEAD-TAB87              PIC X.
           12  HEAD-LF-RET             PIC X(6)  VALUE 'LF RET'.
           12  HEAD-TAB88              PIC X.
           12  HEAD-AH-RET             PIC X(6)  VALUE 'AH RET'.
           12  HEAD-TAB89              PIC X.
           12  HEAD-RET-GRP            PIC X(7)  VALUE 'RET GRP'.
           12  HEAD-TAB90              PIC X.
           12  HEAD-RETRO-EARNINGS.
               16  HEAD-RET-EARN-R     PIC X(6)   VALUE 'REARNR'.
               16  HEAD-TAB91          PIC X.
               16  HEAD-RET-EARN-L     PIC X(6)   VALUE 'REARNL'.
               16  HEAD-TAB92          PIC X.
               16  HEAD-RET-EARN-A     PIC X(6)   VALUE 'REARNA'.
               16  HEAD-TAB93          PIC X.
           12  HEAD-RET-ST-TAX-USE     PIC X(10)  VALUE 'RET ST TAX'.
           12  HEAD-TAB94              PIC X.

           12  HEAD-USER-SELECT-OPTIONS.
               16  HEAD-USER-SELECT-1  PIC XXX     VALUE 'US1'.
               16  HEAD-TAB95          PIC X.
               16  HEAD-USER-SELECT-2  PIC XXX     VALUE 'US2'.
               16  HEAD-TAB96          PIC X.
               16  HEAD-USER-SELECT-3  PIC XXX     VALUE 'US3'.
               16  HEAD-TAB97          PIC X.
               16  HEAD-USER-SELECT-4  PIC XXX     VALUE 'US4'.
               16  HEAD-TAB98          PIC X.
               16  HEAD-USER-SELECT-5  PIC XXX     VALUE 'US5'.
               16  HEAD-TAB99          PIC X.

           12  HEAD-LF-RPT021-EXP-PCT  PIC X(6)    VALUE 'LF EXP'.
           12  HEAD-TAB100             PIC X.

           12  HEAD-AH-RPT021-EXP-PCT  PIC X(6)    VALUE 'AH EXP'.
           12  HEAD-TAB101             PIC X.

           12  HEAD-RPT045A-SWITCH     PIC X(6)    VALUE 'ECS045'.
           12  HEAD-TAB102             PIC X.

           12  HEAD-INSURANCE-LIMITS.
               16  HEAD-MAX-MON-BEN    PIC X(8)    VALUE 'MAX MBEN'.
               16  HEAD-TAB103         PIC X.
               16  HEAD-MAX-TOT-BEN    PIC X(8)    VALUE 'MAX TBEN'.
               16  HEAD-TAB104         PIC X.

           12  HEAD-DISMBR-COVERAGE-SW PIC X(4)    VALUE 'DISM'.
           12  HEAD-TAB105             PIC X.

           12  HEAD-CANCEL-FEE         PIC X(7)    VALUE 'CAN FEE'.
           12  HEAD-TAB106             PIC X.

           12  HEAD-TOL-REF-PCT        PIC X(7)    VALUE 'REF PCT'.
           12  HEAD-TAB107             PIC X.


           12  HEAD-3RD-PARTY-NOTIF-LEVEL  PIC X(8) VALUE '3RD PART'.
           12  HEAD-TAB108             PIC X.
           12  HEAD-NOTIFICATION-TYPES.
               16  HEAD-NOTIF-OF-LETTERS PIC X(5)  VALUE 'LETRS'.
               16  HEAD-TAB109         PIC X.
               16  HEAD-NOTIF-OF-PAYMENTS  PIC X(4) VALUE 'PMTS'.
               16  HEAD-TAB110         PIC X.
               16  HEAD-NOTIF-OF-REPORTS   PIC X(4) VALUE 'RPTS'.
               16  HEAD-TAB111         PIC X.
               16  HEAD-NOTIF-OF-STATUS    PIC X(4) VALUE 'STAT'.
               16  HEAD-TAB112         PIC X.

           12  HEAD-BENEFIT-TABLE-USAGE    PIC X(7) VALUE 'TBL USE'.
           12  HEAD-TAB113             PIC X.

           12  HEAD-ALLOWABLE-BENEFITS OCCURS 10  TIMES.
               16  HEAD-BENEFIT-CODE       PIC X(7) VALUE 'BENCD1 '.
               16  HEAD-TAB114             PIC X.
               16  HEAD-BENEFIT-TYPE       PIC X(7) VALUE 'BENTP1 '.
               16  HEAD-TAB115             PIC X.
               16  HEAD-BENEFIT-REVISION   PIC X(7) VALUE 'BENRV1 '.
               16  HEAD-TAB116             PIC X.
               16  HEAD-BENEFIT-REM-TERM   PIC X(7) VALUE 'BENRT1 '.
               16  HEAD-TAB117             PIC X.
               16  HEAD-BENEFIT-RETRO-Y-N  PIC X(7) VALUE 'BENRE1 '.
               16  HEAD-TAB118             PIC X.

           12  HEAD-CONTROL-NAME           PIC X(7) VALUE 'CON NME'.
           12  HEAD-TAB120                 PIC X.

           12  HEAD-RETRO-QUALIFY-LIMIT PIC X(7) VALUE 'RETQLIM'.
           12  HEAD-TAB121              PIC X.
           12  HEAD-RETRO-PREM-P-E      PIC X(6) VALUE 'RETPPE'.
           12  HEAD-TAB122              PIC X.
           12  HEAD-RETRO-CLMS-P-I      PIC X(6) VALUE 'RETCPI'.
           12  HEAD-TAB123              PIC X.
           12  HEAD-RETRO-RET-METHOD-LF
                                   PIC X(6)  VALUE 'RETMLF'.
           12  HEAD-TAB124               PIC X.
           12  HEAD-RETRO-RET-BASIS-LF
                                   PIC X(6)  VALUE 'RETBLF'.
           12  HEAD-TAB125               PIC X.
           12  HEAD-RETRO-BRACKETS-LF OCCURS 3 TIMES.
               16  HEAD-RETRO-RET-PCT-LF   PIC X(7)  VALUE 'RETPLF1'.
               16  HEAD-TAB126             PIC X.
               16  HEAD-RETRO-RET-THRU-LF  PIC X(7)  VALUE 'RETTLF1'.
               16  HEAD-TAB127             PIC X.
           12  HEAD-RETRO-RET-METHOD-AH  PIC X(6)    VALUE 'RETMAH'.
           12  HEAD-TAB128             PIC X.
           12  HEAD-RETRO-RET-BASIS-AH PIC X(6)    VALUE 'RETBAH'.
           12  HEAD-TAB129             PIC X.
           12  HEAD-RETRO-BRACKETS-AH OCCURS 3 TIMES.
               16  HEAD-RETRO-RET-PCT-AH PIC X(7)    VALUE 'RETPAH1'.
               16  HEAD-TAB130         PIC X.
               16  HEAD-RETRO-RET-THRU-AH PIC X(7)   VALUE 'RETTAH1'.
               16  HEAD-TAB131         PIC X.

           12  HEAD-COMMENTS OCCURS 5 TIMES.
               16  HEAD-COMMENT-LINE   PIC X(8)  VALUE 'COMMENT1'.
               16  HEAD-TAB132         PIC X.
           12  HEAD-REI-GROUP-A        PIC X(9)   VALUE 'REI GRP A'.
           12  HEAD-TAB133             PIC X.
           12  HEAD-REI-GROUP-B        PIC X(9)   VALUE 'REI GRP B'.
           12  HEAD-TAB134             PIC X.
           12  HEAD-FAX-NO             PIC X(6)  VALUE 'FAX NO'.
022703     12  HEAD-TAB135             PIC X.
022703     12  HEAD-DTE-RANGE          PIC X(18)
022703                      VALUE 'CURRENT DATE RANGE'.
      /
       01  WS-MISC.
082603     05  PGM-SUB                 PIC S9(4)   VALUE +548.
082603     05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
082603     05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
082603     05  WS-ZERO                 PIC S9      VALUE ZERO.
082603     05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-SAVE-ERACCT          PIC X(1533) VALUE LOW-VALUES.
           05  ERACCT-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  ERCOMP-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)   VALUE ZEROS.

                                       COPY ELCDATE.

082603                                 COPY ELCDTECX.
082603
082603                                 COPY ELCDTEVR.
082603

           EJECT
       PROCEDURE DIVISION.

082603 0000-LOAD-DATE-CARD.            COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ACCT   THRU 0100-EXIT UNTIL
022703         END-OF-ERACCT 
022703     PERFORM 0100-PROCESS-ACCT   THRU 0100-EXIT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ACCT RECORDS READ    '  ACT-RECS-IN
           DISPLAY ' ACCT RECORDS WRITTEN '  ACT-RECS-OUT
           GOBACK
           .

       0100-PROCESS-ACCT.

           MOVE WS-SAVE-ERACCT         TO ACCT-DETAIL-RECORD
           IF ERACCT-ACCT-KEY = AM-CONTROL-A
              CONTINUE
           ELSE
              MOVE '*'                 TO ACCT-CURRENT-DTE-RANGE
           END-IF
           MOVE AM-CARRIER             TO ACCT-CARRIER
           MOVE AM-GROUPING            TO ACCT-GROUPING
           MOVE AM-STATE               TO ACCT-STATE
           MOVE AM-ACCOUNT             TO ACCT-ACCOUNT

102902     IF AM-EXPIRATION-DT = HIGH-VALUES
102902        MOVE '99/99/9999'        TO ACCT-EXP-DATE
102902     ELSE
102902        MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
102902        MOVE ' '                 TO DC-OPTION-CODE
102902        PERFORM 8510-DATE-CONVERSION
102902                                 THRU 8590-EXIT
102902        IF NO-CONVERSION-ERROR
102902           MOVE DC-GREG-DATE-A-EDIT
102902                                 TO ACCT-EXP-DATE
102902        END-IF
102902     END-IF

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-EFF-DATE
           END-IF

           MOVE AM-REPORT-CODE-1       TO ACCT-REPORT-CODE-1
           MOVE AM-REPORT-CODE-2       TO ACCT-REPORT-CODE-2

           MOVE AM-CITY-CODE           TO ACCT-CITY-CODE
           MOVE AM-COUNTY-PARISH       TO ACCT-COUNTY-PARISH
           MOVE AM-NAME                TO ACCT-NAME
           MOVE AM-PERSON              TO ACCT-PERSON
           MOVE AM-ADDRS               TO ACCT-ADDRS
           MOVE AM-CITY                TO ACCT-CITY
           MOVE AM-ZIP                 TO ACCT-ZIP
           MOVE AM-TEL-NO              TO ACCT-TEL-NO
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +8
101305        IF AM-AGT (SUB1) NOT = SPACES AND ZEROS
                MOVE AM-AGT (SUB1)     TO ACCT-AGT (SUB1)
                MOVE AM-COM-TYP (SUB1) TO ACCT-COM-TYP (SUB1)
053003          IF (AM-L-COM (SUB1) NUMERIC)
053003             AND (AM-L-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                   MOVE AM-L-COM (SUB1)
                                       TO ACCT-L-COM (SUB1)
                ELSE
                   MOVE AM-L-COMA (SUB1)
                                       TO ACCT-L-COMA (SUB1)
                END-IF
053003          IF (AM-J-COM (SUB1) NUMERIC)
053003             AND (AM-J-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                   MOVE AM-J-COM (SUB1)
                                       TO ACCT-J-COM (SUB1)
                ELSE
                   MOVE AM-J-COMA (SUB1)
                                       TO ACCT-J-COMA (SUB1)
                END-IF
053003          IF (AM-A-COM (SUB1) NUMERIC)
053003             AND (AM-A-COMA (SUB1) (3:1) NOT = 'M' AND 'L')
                   MOVE AM-A-COM (SUB1)
                                       TO ACCT-A-COM (SUB1)
                ELSE
                   MOVE AM-A-COMA (SUB1)
                                       TO ACCT-A-COMA (SUB1)
                END-IF
                MOVE AM-RECALC-LV-INDIC (SUB1)
                                       TO ACCT-RECALC-LV-INDIC (SUB1)
                MOVE AM-RETRO-LV-INDIC (SUB1)
                                       TO ACCT-RETRO-LV-INDIC (SUB1)
                MOVE AM-GL-CODES (SUB1)
                                       TO ACCT-GL-CODES (SUB1)
                MOVE AM-COMM-CHARGEBACK (SUB1)
                                       TO ACCT-COMM-CHARGEBACK (SUB1)
              END-IF
           END-PERFORM

           MOVE AM-CSR-CODE            TO ACCT-CSR-CODE
           MOVE AM-GPCD                TO ACCT-GPCD
           MOVE AM-IG                  TO ACCT-IG
           MOVE AM-STATUS              TO ACCT-STATUS
           MOVE AM-REMIT-TO            TO ACCT-REMIT-TO
           MOVE AM-ID-NO               TO ACCT-ID-NO
           MOVE AM-CAL-TABLE           TO ACCT-CAL-TABLE
           MOVE AM-FLD-1               TO ACCT-FLD-1
           MOVE AM-FLD-2               TO ACCT-FLD-2
           MOVE AM-FLD-3               TO ACCT-FLD-3
           MOVE AM-FLD-4               TO ACCT-FLD-4
           MOVE AM-FLD-5               TO ACCT-FLD-5

           MOVE AM-1ST-PROD-DATE       TO DC-GREG-DATE-1-YMD-R
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-1ST-PROD-DATE
           END-IF

           MOVE AM-ANNIVERSARY-DATE    TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-ANNIVERSARY-DATE
           END-IF

           MOVE AM-CERTS-PURGED-DATE   TO DC-GREG-DATE-1-YMD-R
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-CERTS-PURGED-DATE
           END-IF

           MOVE AM-HI-CERT-DATE        TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-HI-CERT-DATE
           END-IF

           MOVE AM-LO-CERT-DATE        TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-LO-CERT-DATE
           END-IF

           MOVE AM-ENTRY-DATE          TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-ENTRY-DATE
           END-IF

           MOVE AM-INACTIVE-DATE       TO DC-GREG-DATE-1-YMD-R
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO ACCT-INACTIVE-DATE
           END-IF

           MOVE AM-AH-ONLY-INDICATOR   TO ACCT-AH-ONLY-INDICATOR
           MOVE AM-EDIT-LOAN-OFC       TO ACCT-EDIT-LOAN-OFC
           IF AM-OVR-SHT-AMT NUMERIC
              MOVE AM-OVR-SHT-AMT      TO ACCT-OVR-SHT-AMT
           END-IF
           IF AM-OVR-SHT-PCT NUMERIC
              MOVE AM-OVR-SHT-PCT      TO ACCT-OVR-SHT-PCT
           END-IF
           MOVE AM-RECALC-COMM         TO ACCT-RECALC-COMM
           MOVE AM-RECALC-REIN         TO ACCT-RECALC-REIN
           MOVE AM-REI-TABLE           TO ACCT-REI-TABLE
           MOVE AM-REI-GROUP-A         TO ACCT-REI-GROUP-A
           MOVE AM-REI-GROUP-B         TO ACCT-REI-GROUP-B
           MOVE AM-REI-LF-TAX          TO ACCT-REI-LF-TAX
           MOVE AM-REI-PR-PCT          TO ACCT-REI-PR-PCT
           MOVE AM-REI-78-PCT          TO ACCT-REI-78-PCT
           MOVE AM-REI-AH-TAX          TO ACCT-REI-AH-TAX
           MOVE AM-STD-AH-TYPE         TO ACCT-STD-AH-TYPE
           MOVE AM-EARN-METHOD-R       TO ACCT-EARN-METHOD-R
           MOVE AM-EARN-METHOD-L       TO ACCT-EARN-METHOD-L
           MOVE AM-EARN-METHOD-A       TO ACCT-EARN-METHOD-A
           MOVE AM-TOL-PREM            TO ACCT-TOL-PREM
           MOVE AM-TOL-REF             TO ACCT-TOL-REF
           MOVE AM-TOL-CLM             TO ACCT-TOL-CLM
           MOVE AM-RET-Y-N             TO ACCT-RET-Y-N
           MOVE AM-RET-P-E             TO ACCT-RET-P-E
           MOVE AM-LF-RET              TO ACCT-LF-RET
           MOVE AM-AH-RET              TO ACCT-AH-RET
           MOVE AM-RET-GRP             TO ACCT-RET-GRP
           MOVE AM-RET-EARN-R          TO ACCT-RET-EARN-R
           MOVE AM-RET-EARN-L          TO ACCT-RET-EARN-L
           MOVE AM-RET-EARN-A          TO ACCT-RET-EARN-A
           MOVE AM-RET-ST-TAX-USE      TO ACCT-RET-ST-TAX-USE
           MOVE AM-USER-SELECT-1       TO ACCT-USER-SELECT-1
           MOVE AM-USER-SELECT-2       TO ACCT-USER-SELECT-2
           MOVE AM-USER-SELECT-3       TO ACCT-USER-SELECT-3
           MOVE AM-USER-SELECT-4       TO ACCT-USER-SELECT-4
           MOVE AM-USER-SELECT-5       TO ACCT-USER-SELECT-5
           IF AM-LF-RPT021-EXP-PCT NUMERIC
              MOVE AM-LF-RPT021-EXP-PCT
                                       TO ACCT-LF-RPT021-EXP-PCT
           END-IF
           IF AM-AH-RPT021-EXP-PCT NUMERIC
              MOVE AM-AH-RPT021-EXP-PCT
                                       TO ACCT-AH-RPT021-EXP-PCT
           END-IF
           MOVE AM-RPT045A-SWITCH      TO ACCT-RPT045A-SWITCH
           IF AM-MAX-MON-BEN NUMERIC
              MOVE AM-MAX-MON-BEN      TO ACCT-MAX-MON-BEN
           END-IF
           IF AM-MAX-TOT-BEN NUMERIC
              MOVE AM-MAX-TOT-BEN      TO ACCT-MAX-TOT-BEN
           END-IF
           MOVE AM-DISMBR-COVERAGE-SW  TO ACCT-DISMBR-COVERAGE-SW
           IF AM-CANCEL-FEE NUMERIC
              MOVE AM-CANCEL-FEE       TO ACCT-CANCEL-FEE
           END-IF
           IF AM-TOL-REF-PCT NUMERIC
              MOVE AM-TOL-REF-PCT      TO ACCT-TOL-REF-PCT
           END-IF
           MOVE AM-3RD-PARTY-NOTIF-LEVEL
                                       TO ACCT-3RD-PARTY-NOTIF-LEVEL
           MOVE AM-NOTIF-OF-LETTERS    TO ACCT-NOTIF-OF-LETTERS
           MOVE AM-NOTIF-OF-PAYMENTS   TO ACCT-NOTIF-OF-PAYMENTS
           MOVE AM-NOTIF-OF-REPORTS    TO ACCT-NOTIF-OF-REPORTS
           MOVE AM-NOTIF-OF-STATUS     TO ACCT-NOTIF-OF-STATUS
           MOVE AM-BENEFIT-TABLE-USAGE TO ACCT-BENEFIT-TABLE-USAGE
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE AM-BENEFIT-CODE (SUB1)
                                       TO ACCT-BENEFIT-CODE (SUB1)
              MOVE AM-BENEFIT-TYPE (SUB1)
                                       TO ACCT-BENEFIT-TYPE (SUB1)
              MOVE AM-BENEFIT-REVISION (SUB1)
                                       TO ACCT-BENEFIT-REVISION (SUB1)
              MOVE AM-BENEFIT-RETRO-Y-N (SUB1)
                                       TO ACCT-BENEFIT-RETRO-Y-N (SUB1)
           END-PERFORM
           IF AM-CONTROL-NAME (1:1) = LOW-VALUES
              MOVE SPACES              TO ACCT-CONTROL-NAME
           ELSE
              MOVE AM-CONTROL-NAME     TO ACCT-CONTROL-NAME
           END-IF
           IF AM-RETRO-QUALIFY-LIMIT NUMERIC
              MOVE AM-RETRO-QUALIFY-LIMIT
                                       TO ACCT-RETRO-QUALIFY-LIMIT
           END-IF
           MOVE AM-RETRO-PREM-P-E      TO ACCT-RETRO-PREM-P-E
           MOVE AM-RETRO-CLMS-P-I      TO ACCT-RETRO-CLMS-P-I
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +3
              IF AM-RETRO-RET-PCT-LF (SUB1) NUMERIC
                 MOVE AM-RETRO-RET-PCT-LF (SUB1)
                                       TO ACCT-RETRO-RET-PCT-LF (SUB1)
              END-IF
              IF AM-RETRO-RET-THRU-LF (SUB1) NUMERIC
                 MOVE AM-RETRO-RET-THRU-LF (SUB1)
                                       TO ACCT-RETRO-RET-THRU-LF (SUB1)
              END-IF
           END-PERFORM

           MOVE AM-RETRO-RET-METHOD-AH TO ACCT-RETRO-RET-METHOD-AH
           MOVE AM-RETRO-RET-BASIS-AH  TO ACCT-RETRO-RET-BASIS-AH
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +3
              IF AM-RETRO-RET-PCT-AH (SUB1) NUMERIC
                 MOVE AM-RETRO-RET-PCT-AH (SUB1)
                                       TO ACCT-RETRO-RET-PCT-AH (SUB1)
              END-IF
              IF AM-RETRO-RET-THRU-AH (SUB1) NUMERIC
                 MOVE AM-RETRO-RET-THRU-AH (SUB1)
                                       TO ACCT-RETRO-RET-THRU-AH (SUB1)
              END-IF
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +5
              MOVE AM-COMMENT-LINE (SUB1)
                                       TO ACCT-COMMENT-LINE (SUB1)
           END-PERFORM

           MOVE AM-CONTROL-PRIMARY     TO CO-CONTROL-PRIMARY
           IF AM-REMIT-TO NOT NUMERIC
              MOVE 01                  TO AM-REMIT-TO
           END-IF
           MOVE AM-AGT (AM-REMIT-TO)   TO CO-RESP-NO 
           MOVE AM-AGT (1)             TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE
           READ ERCOMP
           IF ERCOMP-FILE-STATUS = '10' OR '23'
              DISPLAY ' ERCOMP NOT FOUND ' CO-CONTROL-PRIMARY
           ELSE
              IF ERCOMP-FILE-STATUS = '00'
                 MOVE CO-FAXNO         TO ACCT-FAX-NO
              ELSE
                 DISPLAY ' BAD READ ERCOMP ' ERCOMP-FILE-STATUS '  '
                   CO-CONTROL-PRIMARY
              END-IF
           END-IF

           PERFORM 0300-WRITE-ACCT     THRU 0300-EXIT

022703     IF NOT END-OF-ERACCT
               MOVE ERACCT-IN-RECORD   TO ACCOUNT-MASTER
               PERFORM 0200-READ-ACCT  THRU 0200-EXIT
022703     END-IF

           .

       0100-EXIT.
           EXIT.

       0200-READ-ACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
082603        OR (ERACCT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-ERACCT        TO TRUE
052804        MOVE SPACES              TO ERACCT-ACCT-KEY 
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT READ NEXT ' ERACCT-FILE-STATUS
022703           SET END-OF-ERACCT     TO TRUE
              END-IF
           END-IF

022703     IF NOT END-OF-ERACCT
              ADD 1                    TO ACT-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-ACCT.

           if acct-report-code-1 (1:4) = 'JM&A'
           WRITE ACCT-OUT-REC          FROM ACCT-DETAIL-RECORD
           ADD 1                       TO ACT-RECS-OUT
           end-if

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERACCT ERCOMP
               OUTPUT ACCT-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERACCT ACCT-OUT ERCOMP

           .

       0500-EXIT.
           EXIT.

       0550-START-ACCT.

           MOVE LOW-VALUES             TO ERACCT-CONTROL-PRIMARY
082603     MOVE DTE-CLASIC-COMPANY-CD  TO ERACCT-COMPANY-CD

           START ERACCT KEY IS NOT < ERACCT-CONTROL-PRIMARY

           IF (ERACCT-FILE-STATUS = '10' OR '23')
082603        OR (ERACCT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
022703        SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT START     ' ERACCT-FILE-STATUS
022703           SET END-OF-ERACCT     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ACCT-DETAIL-RECORD
           MOVE X'09'                  TO ACCT-TAB1
                                          ACCT-TAB2
                                          ACCT-TAB3
                                          ACCT-TAB4
                                          ACCT-TAB5
                                          ACCT-TAB6
                                          ACCT-TAB7
                                          ACCT-TAB8
                                          ACCT-TAB9
                                          ACCT-TAB10
                                          ACCT-TAB11
                                          ACCT-TAB12
                                          ACCT-TAB13
                                          ACCT-TAB14
                                          ACCT-TAB15
                                          ACCT-TAB16
                                          ACCT-TAB27
                                          ACCT-TAB28
                                          ACCT-TAB29
                                          ACCT-TAB30
                                          ACCT-TAB31
                                          ACCT-TAB32
                                          ACCT-TAB33
                                          ACCT-TAB42
                                          ACCT-TAB43
                                          ACCT-TAB44
                                          ACCT-TAB45
                                          ACCT-TAB46
                                          ACCT-TAB47
                                          ACCT-TAB48
                                          ACCT-TAB49
                                          ACCT-TAB50
                                          ACCT-TAB51
                                          ACCT-TAB52
                                          ACCT-TAB53
                                          ACCT-TAB54
                                          ACCT-TAB55
                                          ACCT-TAB56
                                          ACCT-TAB57
                                          ACCT-TAB58
                                          ACCT-TAB59
                                          ACCT-TAB60
                                          ACCT-TAB68
                                          ACCT-TAB72
                                          ACCT-TAB73
                                          ACCT-TAB74
                                          ACCT-TAB79
                                          ACCT-TAB80
                                          ACCT-TAB81
                                          ACCT-TAB82
                                          ACCT-TAB83
                                          ACCT-TAB84
                                          ACCT-TAB85
                                          ACCT-TAB86
                                          ACCT-TAB87
                                          ACCT-TAB88
                                          ACCT-TAB89
                                          ACCT-TAB90
                                          ACCT-TAB91
                                          ACCT-TAB92
                                          ACCT-TAB93
                                          ACCT-TAB94
                                          ACCT-TAB95
                                          ACCT-TAB96
                                          ACCT-TAB97
                                          ACCT-TAB98
                                          ACCT-TAB99
                                          ACCT-TAB100
                                          ACCT-TAB101
                                          ACCT-TAB102
                                          ACCT-TAB103
                                          ACCT-TAB104
                                          ACCT-TAB105
                                          ACCT-TAB106
                                          ACCT-TAB107
                                          ACCT-TAB108
                                          ACCT-TAB109
                                          ACCT-TAB110
                                          ACCT-TAB111
                                          ACCT-TAB112
                                          ACCT-TAB113
                                          ACCT-TAB120
                                          ACCT-TAB121
                                          ACCT-TAB122
                                          ACCT-TAB123
                                          ACCT-TAB124
                                          ACCT-TAB125
                                          ACCT-TAB128
                                          ACCT-TAB129
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +8
              MOVE ZEROS               TO ACCT-L-COM (SUB1)
                                          ACCT-J-COM (SUB1)
                                          ACCT-A-COM (SUB1)
              MOVE X'09'               TO ACCT-TAB17 (SUB1)
                                          ACCT-TAB18 (SUB1)
                                          ACCT-TAB19 (SUB1)
                                          ACCT-TAB20 (SUB1)
                                          ACCT-TAB21 (SUB1)
                                          ACCT-TAB22 (SUB1)
                                          ACCT-TAB23 (SUB1)
                                          ACCT-TAB24 (SUB1)
                                          ACCT-TAB25 (SUB1)
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE X'09'               TO ACCT-TAB114 (SUB1)
                                          ACCT-TAB115 (SUB1)
                                          ACCT-TAB116 (SUB1)
                                          ACCT-TAB117 (SUB1)
                                          ACCT-TAB118 (SUB1)
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +3
              MOVE ZEROS               TO ACCT-RETRO-RET-PCT-LF (SUB1)
                                          ACCT-RETRO-RET-THRU-LF (SUB1)
                                          ACCT-RETRO-RET-PCT-AH (SUB1)
                                          ACCT-RETRO-RET-THRU-AH (SUB1)
              MOVE X'09'               TO ACCT-TAB126 (SUB1)
                                          ACCT-TAB127 (SUB1)
                                          ACCT-TAB130 (SUB1)
                                          ACCT-TAB131 (SUB1)
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +5
              MOVE X'09'               TO ACCT-TAB132 (SUB1)
           END-PERFORM
           MOVE X'09'                  TO ACCT-TAB133
                                          ACCT-TAB134
                                          ACCT-TAB135
           MOVE ZEROS                  TO ACCT-OVR-SHT-AMT
                                          ACCT-OVR-SHT-PCT
                                          ACCT-REI-LF-TAX
                                          ACCT-REI-PR-PCT
                                          ACCT-REI-78-PCT
                                          ACCT-REI-AH-TAX
                                          ACCT-TOL-PREM
                                          ACCT-TOL-REF
                                          ACCT-TOL-CLM
                                          ACCT-LF-RET
                                          ACCT-AH-RET
                                          ACCT-LF-RPT021-EXP-PCT
                                          ACCT-AH-RPT021-EXP-PCT
                                          ACCT-MAX-MON-BEN
                                          ACCT-MAX-TOT-BEN
                                          ACCT-CANCEL-FEE
                                          ACCT-TOL-REF-PCT
                                          ACCT-3RD-PARTY-NOTIF-LEVEL
                                          ACCT-RETRO-QUALIFY-LIMIT
           MOVE ACCT-DETAIL-RECORD     TO WS-SAVE-ERACCT

           MOVE X'09'                  TO HEAD-TAB1
                                          HEAD-TAB2
                                          HEAD-TAB3
                                          HEAD-TAB4
                                          HEAD-TAB5
                                          HEAD-TAB6
                                          HEAD-TAB7
                                          HEAD-TAB8
                                          HEAD-TAB9
                                          HEAD-TAB10
                                          HEAD-TAB11
                                          HEAD-TAB12
                                          HEAD-TAB13
                                          HEAD-TAB14
                                          HEAD-TAB15
                                          HEAD-TAB16
                                          HEAD-TAB27
                                          HEAD-TAB28
                                          HEAD-TAB29
                                          HEAD-TAB30
                                          HEAD-TAB31
                                          HEAD-TAB32
                                          HEAD-TAB33
                                          HEAD-TAB42
                                          HEAD-TAB43
                                          HEAD-TAB44
                                          HEAD-TAB45
                                          HEAD-TAB46
                                          HEAD-TAB47
                                          HEAD-TAB48
                                          HEAD-TAB49
                                          HEAD-TAB50
                                          HEAD-TAB51
                                          HEAD-TAB52
                                          HEAD-TAB53
                                          HEAD-TAB54
                                          HEAD-TAB55
                                          HEAD-TAB56
                                          HEAD-TAB57
                                          HEAD-TAB58
                                          HEAD-TAB59
                                          HEAD-TAB60
                                          HEAD-TAB68
                                          HEAD-TAB72
                                          HEAD-TAB73
                                          HEAD-TAB74
                                          HEAD-TAB79
                                          HEAD-TAB80
                                          HEAD-TAB81
                                          HEAD-TAB82
                                          HEAD-TAB83
                                          HEAD-TAB84
                                          HEAD-TAB85
                                          HEAD-TAB86
                                          HEAD-TAB87
                                          HEAD-TAB88
                                          HEAD-TAB89
                                          HEAD-TAB90
                                          HEAD-TAB91
                                          HEAD-TAB92
                                          HEAD-TAB93
                                          HEAD-TAB94
                                          HEAD-TAB95
                                          HEAD-TAB96
                                          HEAD-TAB97
                                          HEAD-TAB98
                                          HEAD-TAB99
                                          HEAD-TAB100
                                          HEAD-TAB101
                                          HEAD-TAB102
                                          HEAD-TAB103
                                          HEAD-TAB104
                                          HEAD-TAB105
                                          HEAD-TAB106
                                          HEAD-TAB107
                                          HEAD-TAB108
                                          HEAD-TAB109
                                          HEAD-TAB110
                                          HEAD-TAB111
                                          HEAD-TAB112
                                          HEAD-TAB113
                                          HEAD-TAB120
                                          HEAD-TAB121
                                          HEAD-TAB122
                                          HEAD-TAB123
                                          HEAD-TAB124
                                          HEAD-TAB125
                                          HEAD-TAB128
                                          HEAD-TAB129
                                          HEAD-TAB133
                                          HEAD-TAB134
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +8
              MOVE X'09'               TO HEAD-TAB17 (SUB1)
                                          HEAD-TAB18 (SUB1)
                                          HEAD-TAB19 (SUB1)
                                          HEAD-TAB20 (SUB1)
                                          HEAD-TAB21 (SUB1)
                                          HEAD-TAB22 (SUB1)
                                          HEAD-TAB23 (SUB1)
                                          HEAD-TAB24 (SUB1)
                                          HEAD-TAB25 (SUB1)
           END-PERFORM

           MOVE +1                     TO SUB1
           MOVE 'AGT1 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP1 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM1 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM1 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM1 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL1 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET1 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL1 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK1 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +2                     TO SUB1
           MOVE 'AGT2 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP2 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM2 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM2 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM2 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL2 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET2 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL2 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK2 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +3                     TO SUB1
           MOVE 'AGT3 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP3 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM3 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM3 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM3 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL3 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET3 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL3 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK3 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +4                     TO SUB1
           MOVE 'AGT4 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP4 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM4 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM4 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM4 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL4 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET4 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL4 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK4 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +5                     TO SUB1
           MOVE 'AGT5 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP5 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM5 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM5 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM5 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL5 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET5 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL5 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK5 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +6                     TO SUB1
           MOVE 'AGT6 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP6 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM6 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM6 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM6 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL6 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET6 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL6 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK6 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +7                     TO SUB1
           MOVE 'AGT7 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP7 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM7 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM7 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM7 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL7 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET7 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL7 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK7 '               TO HEAD-COMM-CHARGEBACK (SUB1)

           MOVE +8                     TO SUB1
           MOVE 'AGT8 '                TO HEAD-AGT      (SUB1)
           MOVE 'TYP8 '                TO HEAD-COM-TYP  (SUB1)
           MOVE 'L COM8 '              TO HEAD-L-COM    (SUB1)
           MOVE 'J COM8 '              TO HEAD-J-COM    (SUB1)
           MOVE 'A COM8 '              TO HEAD-A-COM    (SUB1)
           MOVE 'RCAL8 '               TO HEAD-RECALC-LV-INDIC (SUB1)
           MOVE 'RET8 '                TO HEAD-RETRO-LV-INDIC (SUB1)
           MOVE 'GL8 '                 TO HEAD-GL-CODES (SUB1)
           MOVE 'CHBK8 '               TO HEAD-COMM-CHARGEBACK (SUB1)



           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +10
              MOVE X'09'               TO HEAD-TAB114 (SUB1)
                                          HEAD-TAB115 (SUB1)
                                          HEAD-TAB116 (SUB1)
                                          HEAD-TAB117 (SUB1)
                                          HEAD-TAB118 (SUB1)
           END-PERFORM
           MOVE +1                     TO SUB1
           MOVE 'BENCD1 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP1 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV1 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT1 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE1 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +2                     TO SUB1
           MOVE 'BENCD2 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP2 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV2 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT2 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE2 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +3                     TO SUB1
           MOVE 'BENCD3 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP3 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV3 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT3 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE3 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +4                     TO SUB1
           MOVE 'BENCD4 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP4 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV4 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT4 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE4 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +5                     TO SUB1
           MOVE 'BENCD5 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP5 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV5 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT5 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE5 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +6                     TO SUB1
           MOVE 'BENCD6 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP6 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV6 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT6 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE6 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +7                     TO SUB1
           MOVE 'BENCD7 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP7 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV7 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT7 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE7 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +8                     TO SUB1
           MOVE 'BENCD8 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP8 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV8 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT8 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE8 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +9                     TO SUB1
           MOVE 'BENCD9 '              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP9 '              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV9 '              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT9 '              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE9 '              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)

           MOVE +10                    TO SUB1
           MOVE 'BENCD10'              TO HEAD-BENEFIT-CODE (SUB1)
           MOVE 'BENTP10'              TO HEAD-BENEFIT-TYPE (SUB1)
           MOVE 'BENRV10'              TO HEAD-BENEFIT-REVISION (SUB1)
           MOVE 'BENRT10'              TO HEAD-BENEFIT-REM-TERM (SUB1)
           MOVE 'BENRE10'              TO HEAD-BENEFIT-RETRO-Y-N (SUB1)


           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +3
              MOVE X'09'               TO HEAD-TAB126 (SUB1)
                                          HEAD-TAB127 (SUB1)
                                          HEAD-TAB130 (SUB1)
                                          HEAD-TAB131 (SUB1)
           END-PERFORM
           MOVE +1                     TO SUB1
           MOVE 'RETPLF1'              TO HEAD-RETRO-RET-PCT-LF (SUB1)
           MOVE 'RETTLF1'              TO HEAD-RETRO-RET-THRU-LF (SUB1)
           MOVE 'RETPAH1'              TO HEAD-RETRO-RET-PCT-AH (SUB1)
           MOVE 'RETTAH1'              TO HEAD-RETRO-RET-THRU-AH (SUB1)

           MOVE +2                     TO SUB1
           MOVE 'RETPLF2'              TO HEAD-RETRO-RET-PCT-LF (SUB1)
           MOVE 'RETTLF2'              TO HEAD-RETRO-RET-THRU-LF (SUB1)
           MOVE 'RETPAH2'              TO HEAD-RETRO-RET-PCT-AH (SUB1)
           MOVE 'RETTAH2'              TO HEAD-RETRO-RET-THRU-AH (SUB1)

           MOVE +3                     TO SUB1
           MOVE 'RETPLF3'              TO HEAD-RETRO-RET-PCT-LF (SUB1)
           MOVE 'RETTLF3'              TO HEAD-RETRO-RET-THRU-LF (SUB1)
           MOVE 'RETPAH3'              TO HEAD-RETRO-RET-PCT-AH (SUB1)
           MOVE 'RETTAH3'              TO HEAD-RETRO-RET-THRU-AH (SUB1)

           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              SUB1 > +5
              MOVE X'09'               TO HEAD-TAB132 (SUB1)
           END-PERFORM

           MOVE +1                     TO SUB1
           MOVE 'COMMENT1'             TO HEAD-COMMENT-LINE (SUB1)
           MOVE +2                     TO SUB1
           MOVE 'COMMENT2'             TO HEAD-COMMENT-LINE (SUB1)
           MOVE +3                     TO SUB1
           MOVE 'COMMENT3'             TO HEAD-COMMENT-LINE (SUB1)
           MOVE +4                     TO SUB1
           MOVE 'COMMENT4'             TO HEAD-COMMENT-LINE (SUB1)
           MOVE +5                     TO SUB1
           MOVE 'COMMENT5'             TO HEAD-COMMENT-LINE (SUB1)

           WRITE ACCT-HEAD-REC         FROM ACCT-HEADER-RECORD

           PERFORM 0550-START-ACCT     THRU 0550-EXIT
           PERFORM 0200-READ-ACCT      THRU 0200-EXIT
022703     MOVE ERACCT-IN-RECORD       TO ACCOUNT-MASTER
022703     PERFORM 0200-READ-ACCT      THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.
082603 ABEND-PGM SECTION.              COPY ELCABEND.

