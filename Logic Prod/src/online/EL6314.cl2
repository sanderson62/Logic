       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL6314.
      *
      *AUTHOR.        PABLO
      *               OMAHA, NEBRASKA

      *DATE-COMPILED.

      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CNETRAL STATES  *
      *            *   HEALTH AND LIFE                                 *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                   *
      *            *****************************************************
120313*-----------------------------------------------------------------
120313*                   C H A N G E   L O G
120313*
120313* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120313*-----------------------------------------------------------------
120313*  CHANGE  CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120313* EFFECTIVE    NUMBER
120313*-----------------------------------------------------------------
120313* 120313   2013090300001  AJRA  NEW PROGRAM FOR ENDORSEMENT REVIEW 
120313*                               AND SUPERVISOR CORRECTION
122313* 122313   2013090300001  AJRA  CANCEL ENDORSEMENT
021214* 021214   2014021200001  AJRA  FIX FINALIZE FLAG
041320* 041320 CR2020030500002  PEMA  Issue, cancel billing notes
120313*-----------------------------------------------------------------

      *REMARKS.
      *        TRANSACTION - EXBA - ENDORSEMENT REVIEW
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6314 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
       77  WS-SEQ-NO                   PIC 9(4) BINARY.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  WS-LF-ABBRV                 PIC X(10) VALUE SPACES.
       77  WS-LF-DESC                  PIC X(02) VALUE SPACES.
       77  WS-WAIT-PER                 PIC X(03) VALUE ZEROS.
       77  WS-RET-ELIM                 PIC X(01) VALUE SPACES.
       77  WS-AH-DESC                  PIC X(35) VALUE SPACES.
       77  WS-BEN-DAYS                 PIC X(02) VALUE ZEROS.
       77  WS-LF-BENCD                 PIC XX    VALUE '00'.
       77  WS-AH-BENCD                 PIC XX    VALUE '00'.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  WS-KIXHOST                  PIC X(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).


           COPY ELCSCTM.

           COPY ELCSCRTY.

       01  ELCNTL-KEY.
           05  CNTL-COMP-ID            PIC X(3)  VALUE SPACES.
           05  CNTL-REC-TYPE           PIC X     VALUE SPACES.
           05  CNTL-ACCESS.
               10  CNTL-STATE          PIC XX    VALUE SPACES.
               10  CNTL-BEN-CD.
                   15  F               PIC X.
                   15  CNTL-CARR       PIC X.
           05  CNTL-SEQ                PIC S9(4) VALUE +0 COMP.

       01  STANDARD-AREAS.
           12  TIME-MT                 PIC S9(7).
           12  TIME-MT-R  REDEFINES TIME-MT.
               16  FILLER              PIC X.
               16  TIME-LMT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  GETMAIN-SPACE           PIC X VALUE SPACES.
           12  ERENDT-LENGTH           PIC S9(04) VALUE +579 COMP.
           12  ERPNDB-LENGTH           PIC 9(04)  VALUE 585 COMP.
           12  ERPNDB2-KEY.
               16  PNDB2-COMPANY-CD    PIC X.
               16  PNDB2-CARRIER       PIC X.
               16  PNDB2-GROUPING      PIC X(6).
               16  PNDB2-STATE         PIC XX.
               16  PNDB2-ACCOUNT       PIC X(10).
               16  PNDB2-CERT-EFF-DT   PIC XX.
               16  PNDB2-CERT-PRIME    PIC X(10).
               16  PNDB2-CERT-SFX      PIC X.
               16  PNDB2-CHG-SEQ-NO    PIC 9(4) BINARY.
               16  PNDB2-RECORD-TYPE   PIC X.
           12  ERENDT-KEY.
               16  ENDT-COMPANY-CD       PIC X.
               16  ENDT-CARRIER          PIC X.
               16  ENDT-GROUPING         PIC X(6).
               16  ENDT-STATE            PIC XX.
               16  ENDT-ACCOUNT          PIC X(10).
               16  ENDT-CERT-EFF-DT      PIC XX.
               16  ENDT-CERT-PRIME       PIC X(10).
               16  ENDT-CERT-SFX         PIC X.
               16  ENDT-RECORD-TYPE      PIC X.
               16  ENDT-SEQ-NO           PIC 9(4) BINARY.
           12  ELCRTO-KEY.
               16  CRTO-COMPANY-CD     PIC X.
               16  CRTO-CARRIER        PIC X.
               16  CRTO-GROUPING       PIC X(6).
               16  CRTO-STATE          PIC XX.
               16  CRTO-ACCOUNT        PIC X(10).
               16  CRTO-CERT-EFF-DT    PIC XX.
               16  CRTO-CERT-PRIME     PIC X(10).
               16  CRTO-CERT-SFX       PIC X.
               16  CRTO-RECORD-TYPE    PIC X.
               16  CRTO-SEQ-NO         PIC 9(4) BINARY.
           12  WS-ELCERT-KEY.
               16  CERT-COMPANY-CD     PIC X.
               16  CERT-CARRIER        PIC X.
               16  CERT-GROUPING       PIC X(6).
               16  CERT-STATE          PIC XX.
               16  CERT-ACCOUNT        PIC X(10).
               16  CERT-CERT-EFF-DT    PIC XX.
               16  CERT-CERT-PRIME     PIC X(10).
               16  CERT-CERT-SFX       PIC X.
           12  WS-ELCRTT-KEY.
               16  WS-ELCRTT-PRIMARY       PIC X(33).
               16  WS-ELCRTT-REC-TYPE      PIC X(1).
           12  WS-ERMAIL-KEY.
               16  MAIL-COMPANY-CD     PIC X.
               16  MAIL-CARRIER        PIC X.
               16  MAIL-GROUPING       PIC X(6).
               16  MAIL-STATE          PIC XX.
               16  MAIL-ACCOUNT        PIC X(10).
               16  MAIL-CERT-EFF-DT    PIC XX.
               16  MAIL-CERT-PRIME     PIC X(10).
               16  MAIL-CERT-SFX       PIC X.
           12  WS-LF-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-AH-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-CURRENT-DT.
               16  FILLER              PIC X(6)    VALUE SPACES.
               16  WS-CURRENT-YR       PIC XX      VALUE SPACES.
           12  WS-CURRENT-BIN-DT       PIC XX    VALUE LOW-VALUES.
           12  WS-ISS-CAN-SW           PIC X     VALUE LOW-VALUES.
               88  CANCEL-REC            VALUE 'C'.
               88  ISSUE-REC             VALUE 'I'.
           12  WS-WORK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-HOLD-ACCT-KEY        PIC X(26) VALUE LOW-VALUES.

           12  ERACCT-KEY.
               16  ERACCT-COMP-KEY.
                   20  ACCT-CO         PIC X     VALUE SPACES.
                   20  ACCT-CARRIER    PIC X     VALUE SPACES.
                   20  ACCT-GROUPING   PIC X(6)  VALUE SPACES.
                   20  ACCT-STATE      PIC XX    VALUE SPACES.
                   20  ACCT-ACCOUNT    PIC X(10) VALUE SPACES.
               16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.
               16  FILLER              PIC X(4)  VALUE LOW-VALUES.
           12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.
           12  WS-ELCERT-FILE-ID       PIC X(8) VALUE 'ELCERT'.
           12  WS-ELCNTL-FILE-ID       PIC X(8) VALUE 'ELCNTL'.
           12  WS-ELCRTO-FILE-ID       PIC X(8) VALUE 'ELCRTO'.
           12  WS-ERENDT-FILE-ID       PIC X(8) VALUE 'ERENDT'.
           12  WS-ERPNDB2-FILE-ID      PIC X(8) VALUE 'ERPNDB2'.
           12  WS-ERPNDB-FILE-ID       PIC X(8) VALUE 'ERPNDB'.
           12  WS-ERACCT2-FILE-ID      PIC X(8) VALUE 'ERACCT2'.
           12  WS-ELCRTT-FILE-ID       PIC X(8) VALUE 'ELCRTT'.
           12  WS-ERMAIL-FILE-ID       PIC X(8) VALUE 'ERMAIL'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  MAP-NAME            PIC X(8)    VALUE 'EL631M'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL6314S'.
           12  SCREEN-NUMBER       PIC X(4)    VALUE '631M'.
           12  TRANS-ID            PIC X(4)    VALUE 'EXBA'.
           12  THIS-PGM            PIC X(8)    VALUE 'EL6314'.
           12  PGM-NAME            PIC X(8).
           12  TIME-IN             PIC S9(7).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER          PIC X.
               16  TIME-OUT        PIC 99V99.
               16  FILLER          PIC X(2).
           12  XCTL-005            PIC X(8)    VALUE 'EL005'.
           12  XCTL-010            PIC X(8)    VALUE 'EL010'.
           12  XCTL-626            PIC X(8)    VALUE 'EL626'.
           12  XCTL-6311           PIC X(8)    VALUE 'EL6311'.
122313     12  XCTL-6316           PIC X(8)    VALUE 'EL6316'.
           12  XCTL-6317           PIC X(8)    VALUE 'EL6317'.
           12  XCTL-6318           PIC X(8)    VALUE 'EL6318'.
           12  LINK-001            PIC X(8)    VALUE 'EL001'.
           12  LINK-004            PIC X(8)    VALUE 'EL004'.
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
           12  WS-PHONE.
               16  WS-PH1              PIC XXX.
               16  WS-PH2              PIC XXX.
               16  WS-PH3              PIC XXXX.
           12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
           12  WS-DEV-RT               PIC S9V9(6) COMP-3.
           12  WS-TMPDATE.
               16  FILLER              PIC X(2).
               16  WS-TMPDT            PIC X(6).
           12  WS-AMTX                 PIC X(12).
           12  WS-AMTN  REDEFINES WS-AMTX PIC 9(12).
           12  WS-AMTV2 REDEFINES WS-AMTN PIC 9(10)V99.

       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0008                 PIC X(4)  VALUE '0008'.
           12  ER-0023                 PIC X(4)  VALUE '0023'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0132                 PIC X(4)  VALUE '0132'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0148                 PIC X(4)  VALUE '0148'.
           12  ER-0215                 PIC X(4)  VALUE '0215'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
           12  ER-1101                 PIC X(4)  VALUE '1101'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2079                 PIC X(4)  VALUE '2079'.
           12  ER-2208                 PIC X(4)  VALUE '2208'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2253                 PIC X(4)  VALUE '2253'.
           12  ER-2619                 PIC X(4)  VALUE '2619'.
           12  ER-2651                 PIC X(4)  VALUE '2651'.
           12  ER-2784                 PIC X(4)  VALUE '2784'.
           12  ER-2785                 PIC X(4)  VALUE '2785'.
           12  ER-2786                 PIC X(4)  VALUE '2786'.
           12  ER-2791                 PIC X(4)  VALUE '2791'.
           12  ER-2845                 PIC X(4)  VALUE '2845'.
           12  ER-2848                 PIC X(4)  VALUE '2848'.
           12  ER-2851                 PIC X(4)  VALUE '2851'.
           12  ER-2883                 PIC X(4)  VALUE '2883'.
           12  ER-3831                 PIC X(4)  VALUE '3831'.
           12  ER-3836                 PIC X(4)  VALUE '3836'.
           12  ER-3837                 PIC X(4)  VALUE '3837'.
           12  ER-3839                 PIC X(4)  VALUE '3839'.
           12  ER-7638                 PIC X(4)  VALUE '7638'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.

           COPY ELCDATE.

           COPY ELCLOGOF.

           COPY ELCATTR.

           COPY ELCEMIB.

           COPY ELCINTF.
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
              16  PI-ISS-CAN-SW           PIC X.
              16  PI-ERENDT-KEY.
                  20  PI-ENDT-COMPANY-CD  PIC X.
                  20  PI-ENDT-CARRIER     PIC X.
                  20  PI-ENDT-GROUPING    PIC X(6).
                  20  PI-ENDT-STATE       PIC XX.
                  20  PI-ENDT-ACCOUNT     PIC X(10).
                  20  PI-ENDT-CERT-EFF-DT PIC XX.
                  20  PI-ENDT-CERT-PRIME  PIC X(10).
                  20  PI-ENDT-CERT-SFX    PIC X.
                  20  PI-ENDT-RECORD-TYPE PIC X.
                  20  PI-ENDT-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ELCRTO-KEY.
                  20  PI-CRTO-COMPANY-CD  PIC X.
                  20  PI-CRTO-CARRIER     PIC X.
                  20  PI-CRTO-GROUPING    PIC X(6).
                  20  PI-CRTO-STATE       PIC XX.
                  20  PI-CRTO-ACCOUNT     PIC X(10).
                  20  PI-CRTO-CERT-EFF-DT PIC XX.
                  20  PI-CRTO-CERT-PRIME  PIC X(10).
                  20  PI-CRTO-CERT-SFX    PIC X.
                  20  PI-CRTO-RECORD-TYPE PIC X.
                  20  PI-CRTO-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ERPNDB-KEY           PIC X(11).
              16  PI-DOCUMENT-PROCESSED   PIC X.
                  88  PI-VERI-PROCESSED      VALUE 'V'.
                  88  PI-ENDO-PROCESSED      VALUE 'G'.
              16  PI-PREV-ERENDT-KEY      PIC X(36).
              16  PI-RES-REF-CLM-TYPE     PIC X.
              16  PI-CLM-REFORM-IND       PIC X.
                  88  PI-CLAIM-REFORMATION   VALUE 'Y'.
              16  PI-PASS-AREA.
                  20  FILLER              PIC X(237).
                  20  PI-CHANGES-MADE     PIC X.
                  20  PI-AMT-CHANGES-MADE PIC X.
                  20  PI-PF3-OK           PIC X.
122313            20  PI-CERT-HAS-CANCEL  PIC X.
122313            20  PI-ENDORSE-RECORD   PIC X.
122313            20  FILLER              PIC X(276).
122313        16  PI-6316-PASS-AREA REDEFINES PI-PASS-AREA.
122313            20  PI-6316-DATA        PIC X(93).
122313            20  FILLER              PIC X(143).
122313            20  PI-6316-FINALIZED   PIC X.
122313            20  FILLER              PIC X(280).
              16  PI-6317-PASS-AREA REDEFINES PI-PASS-AREA.
                  20  PI-6317-DATA        PIC X(93).
                  20  FILLER              PIC X(143).
                  20  PI-6317-FINALIZED   PIC X.
                  20  FILLER              PIC X(280).
              16  PI-6318-PASS-AREA REDEFINES PI-PASS-AREA.
021214            20  PI-6318-DATA        PIC X(235).
                  20  PI-6318-FINALIZED   PIC X.
021214            20  FILLER              PIC X(253).
                  20  PI-6318-REASON-IND  PIC X(26).
                  20  FILLER              PIC X(2).
          12  FILLER                      PIC X(276).

       01  WS-REASON-CODE-IND.
           12  WS-REASON-A                PIC X  VALUE SPACES.
           12  WS-REASON-B                PIC X  VALUE SPACES.
           12  WS-REASON-C                PIC X  VALUE SPACES.
           12  WS-REASON-D                PIC X  VALUE SPACES.
           12  WS-REASON-E                PIC X  VALUE SPACES.
           12  WS-REASON-F                PIC X  VALUE SPACES.
           12  WS-REASON-G                PIC X  VALUE SPACES.
           12  WS-REASON-H                PIC X  VALUE SPACES.
           12  WS-REASON-I                PIC X  VALUE SPACES.
           12  WS-REASON-J                PIC X  VALUE SPACES.
           12  WS-REASON-K                PIC X  VALUE SPACES.
           12  WS-REASON-L                PIC X  VALUE SPACES.
           12  WS-REASON-M                PIC X  VALUE SPACES.
           12  WS-REASON-N                PIC X  VALUE SPACES.
           12  WS-REASON-O                PIC X  VALUE SPACES.
           12  WS-REASON-P                PIC X  VALUE SPACES.
           12  WS-REASON-Q                PIC X  VALUE SPACES.
           12  WS-REASON-R                PIC X  VALUE SPACES.
           12  WS-REASON-S                PIC X  VALUE SPACES.
           12  WS-REASON-T                PIC X  VALUE SPACES.
           12  WS-REASON-U                PIC X  VALUE SPACES.
           12  WS-REASON-V                PIC X  VALUE SPACES.
           12  WS-REASON-W                PIC X  VALUE SPACES.
           12  WS-REASON-X                PIC X  VALUE SPACES.
           12  WS-REASON-Y                PIC X  VALUE SPACES.
           12  WS-REASON-Z                PIC X  VALUE SPACES.
      
           COPY ELCAID.

       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24 TIMES.

          COPY EL6314S.

       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(1300).
       01  var  pic x(30).
                                       COPY ERCACCT.
                                       COPY ELCCNTL.
                                       COPY ELCCERT.
                                       COPY ELCCRTO.
                                       COPY ERCENDT.
                                       COPY ERCMAIL.
                                       COPY ELCCRTT.

       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT
           MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT

           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
      *     display ' entering EL6314 '
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
      *       DISPLAY ' WS KIX SYS ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXHOST = ' var (1:env-var-len)
              MOVE var(1:env-var-len)  to ws-kixhost
      *       DISPLAY ' WS KIX HOST ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           MOVE LOW-VALUES             TO EL631MI

           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM             TO PI-CALLING-PROGRAM
                 GO TO 0350-DISPLAY-COMMON-DATA
              ELSE
               IF (PI-CALLING-PROGRAM = 'EL6318' AND 
                   PI-6318-FINALIZED = 'Y')   OR
                  (PI-CALLING-PROGRAM = 'EL6317' AND 
                   PI-6317-FINALIZED = 'Y')
                 MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM  
                 MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1  
                 MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2  
                 MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3  
                 MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4  
                 MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5  
                 MOVE SPACES                 TO PI-SAVED-PROGRAM-6
                 GO TO 9400-CLEAR
               ELSE
                 MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM  
                 MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1  
                 MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2  
                 MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3  
                 MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4  
                 MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5  
                 MOVE SPACES                 TO PI-SAVED-PROGRAM-6
                 IF PI-VERI-PROCESSED OR PI-ENDO-PROCESSED
                    GO TO 0330-SHOW-ERENDT
                 END-IF
                 GO TO 0350-DISPLAY-COMMON-DATA
               END-IF
              END-IF
           END-IF

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR
           END-IF

           .
       0200-RECEIVE.
      *     display 'made it to 0200'
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE ER-0008             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO PFENTRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           EXEC CICS RECEIVE
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               INTO     (EL631MI)
           END-EXEC

           IF PFENTRL = 0
               GO TO 0300-CHECK-PFKEYS
           END-IF
           IF EIBAID NOT = DFHENTER
               MOVE ER-0004            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR
           END-IF

           IF (PFENTRI NUMERIC) AND (PFENTRI > 0 AND < 25)
              MOVE PF-VALUES (PFENTRI) TO EIBAID
           ELSE
              MOVE ER-0029            TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF

           .
       0300-CHECK-PFKEYS.
      *      display 'made it to 0300' 

           IF EIBAID NOT EQUAL DFHPF3
               MOVE 'N' TO PI-PF3-OK
           END-IF
           IF EIBAID = DFHPF1
              MOVE PI-ERENDT-KEY       TO ERENDT-KEY
              PERFORM 0520-ERENDT-STARTBR
                                       THRU 0520-EXIT
              PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
              IF ERENDT-KEY = PI-PREV-ERENDT-KEY
                 PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
              END-IF
              IF (RESP-NORMAL) AND
                 (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
                 PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
122313           MOVE 'Y'              TO PI-ENDORSE-RECORD
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
                 MOVE ER-2237          TO EMI-ERROR
                 MOVE -1               TO CARRL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF EIBAID = DFHPF2
              MOVE PI-ERENDT-KEY       TO ERENDT-KEY
              PERFORM 0520-ERENDT-STARTBR
                                       THRU 0520-EXIT
              PERFORM 0510-ERENDT-READNEXT
                                       THRU 0510-EXIT
              PERFORM 0530-ERENDT-READPREV
                                       THRU 0530-EXIT
              PERFORM 0530-ERENDT-READPREV
                                       THRU 0530-EXIT
              IF (RESP-NORMAL) AND
                 (PI-ERENDT-KEY (1:34) = EN-CONTROL-PRIMARY (1:34))
                 PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
122313           MOVE 'Y'              TO PI-ENDORSE-RECORD
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
122313           MOVE 'N'              TO PI-ENDORSE-RECORD
122313           MOVE LOW-VALUES       TO  EL631MI
                 GO TO 0350-DISPLAY-COMMON-DATA
              END-IF
           END-IF
           
           IF EIBAID = DFHPF4
            AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
               IF PI-CHANGES-MADE = 'N'
                 MOVE ER-3837          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF EIBAID = DFHPF3
            AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
               IF PI-AMT-CHANGES-MADE = 'Y' AND
                PI-PF3-OK NOT = 'Y'
                 MOVE 'Y'              TO  PI-PF3-OK
                 MOVE ER-3839          TO  EMI-ERROR
                 MOVE -1               TO  CARRL
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF EIBAID = DFHPF3
            AND PI-PROCESSOR-IS-CSR
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
              MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
              PERFORM 0355-READ-CERT THRU 0355-EXIT
              PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
              PERFORM 0375-RESET-CRTO-REC THRU 0375-EXIT
              MOVE LOW-VALUES             TO PI-6317-PASS-AREA
              MOVE XCTL-6317              TO PGM-NAME
              GO TO 9300-XCTL
           END-IF

           IF EIBAID = DFHPF4
            AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
              MOVE PI-6318-REASON-IND     TO WS-REASON-CODE-IND
              MOVE LOW-VALUES             TO PI-6318-PASS-AREA
              MOVE WS-REASON-CODE-IND     TO PI-6318-REASON-IND
              MOVE XCTL-6318              TO PGM-NAME
              GO TO 9300-XCTL
           END-IF


           IF EIBAID = DFHPF6
            AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
               MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
               PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
               PERFORM 0355-READ-CERT THRU 0355-EXIT
               PERFORM 0375-RESET-CRTO-REC THRU 0375-EXIT
               PERFORM 0370-MOVE-CERT-TO-SCREEN THRU 0370-EXIT
               PERFORM 0400-COMPARE-FROM-TO THRU 0400-EXIT
               MOVE ER-0000          TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8100-SEND-INITIAL-MAP
           END-IF
122313
122313     IF EIBAID = DFHPF7
122313      AND PI-PROCESSOR-IS-CSR
122313      AND PI-CERT-HAS-CANCEL = 'Y'
122313        MOVE '2'                    TO PI-ISS-CAN-SW
122313        MOVE LOW-VALUES             TO PI-6316-PASS-AREA
122313        MOVE XCTL-6316              TO PGM-NAME
122313        GO TO 9300-XCTL
122313     END-IF

           IF EIBAID = DFHPF23
              GO TO 8810-PF23
           END-IF

           IF EIBAID = DFHPF24
              GO TO 9200-RETURN-MAIN-MENU
           END-IF

           IF EIBAID = DFHPF12
              GO TO 9500-PF12
           END-IF

           IF EIBAID = DFHENTER
            AND PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
               MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
               PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
               PERFORM 0385-CHG-CRTO-REC THRU 0385-EXIT
               PERFORM 0355-READ-CERT THRU 0355-EXIT
               PERFORM 0370-MOVE-CERT-TO-SCREEN THRU 0370-EXIT
               PERFORM 0400-COMPARE-FROM-TO THRU 0400-EXIT
               MOVE ER-0000          TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8100-SEND-INITIAL-MAP
           END-IF

           MOVE ER-0029                TO EMI-ERROR

           .
       0320-INPUT-ERROR.

           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           MOVE AL-UNBON               TO PFENTRA
           MOVE -1                     TO PFENTRL
           GO TO 8200-SEND-DATAONLY

           .
       0325-EDIT-SCREEN.

           MOVE PI-COMPANY-CD          TO ENDT-COMPANY-CD


           IF CRTSFXL > +0
              MOVE CRTSFXI             TO ENDT-CERT-SFX
              MOVE AL-UANON            TO CRTSFXA
           END-IF

           MOVE +0                     TO ENDT-SEQ-NO
           MOVE 'I'                    TO ENDT-RECORD-TYPE

           IF EFFDTL > +0
              EXEC CICS BIF DEEDIT
                   FIELD   (EFFDTI)
                   LENGTH  (8)
              END-EXEC
              MOVE EFFDTI              TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF DATE-CONVERSION-ERROR
                 MOVE ER-0215          TO EMI-ERROR
                 MOVE -1               TO EFFDTL
                 MOVE AL-UABON         TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 MOVE DC-BIN-DATE-1    TO ENDT-CERT-EFF-DT
                 MOVE AL-UANON         TO EFFDTA
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO EFFDTO
              END-IF
           END-IF

           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0325-EXIT.
           EXIT.

       0330-SHOW-ERENDT.

      *     display ' made it to 0330 show erendt '

           MOVE PI-ERENDT-KEY          TO ERENDT-KEY

           PERFORM 0365-READ-ERENDT    THRU 0365-EXIT

           IF RESP-NORMAL
              PERFORM 0390-BUILD-FROM-ERENDT
                                       THRU 0390-EXIT
           ELSE
              IF RESP-NOTFND OR RESP-ENDFILE
                 GO TO 0350-DISPLAY-COMMON-DATA
                 MOVE ER-2253          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           GO TO 8100-SEND-INITIAL-MAP

           .
       0330-EXIT.
           EXIT.

       0350-DISPLAY-COMMON-DATA.

      *     display ' made it to 0350 '
           PERFORM 0355-READ-CERT THRU 0355-EXIT
           PERFORM 0360-CHECK-OLD-ORIG THRU 0360-EXIT
           PERFORM 0370-MOVE-CERT-TO-SCREEN THRU 0370-EXIT
           PERFORM 0400-COMPARE-FROM-TO THRU 0400-EXIT
           MOVE ER-2784                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 8100-SEND-INITIAL-MAP

           .
       0350-EXIT.
           EXIT.


       0355-READ-CERT.
      *     display 'made it to 0355'
           MOVE PI-COMPANY-CD          TO CERT-COMPANY-CD
           MOVE PI-CARRIER             TO CERT-CARRIER
           MOVE PI-GROUPING            TO CERT-GROUPING
           MOVE PI-STATE               TO CERT-STATE
           MOVE PI-ACCOUNT             TO CERT-ACCOUNT
           MOVE PI-CERT-PRIME          TO CERT-CERT-PRIME
           MOVE PI-CERT-SFX            TO CERT-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CERT-CERT-EFF-DT

           EXEC CICS READ
               DATASET   (WS-ELCERT-FILE-ID)
               SET       (ADDRESS OF CERTIFICATE-MASTER)
               RIDFLD    (WS-ELCERT-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              CONTINUE
           ELSE
              IF RESP-NOTFND
                 MOVE LOW-VALUES       TO EL631MI
                 MOVE ER-2253          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO CARRL
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           .
       0355-EXIT.
           EXIT.


       0360-CHECK-OLD-ORIG.

      *     display ' made it to 0360 '

           MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
           MOVE PI-CARRIER             TO CRTO-CARRIER
           MOVE PI-GROUPING            TO CRTO-GROUPING
           MOVE PI-STATE               TO CRTO-STATE
           MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
           MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
           MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
           MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
           MOVE 'I'                    TO CRTO-RECORD-TYPE
           MOVE ZEROS                  TO CRTO-SEQ-NO
           MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
                                          PI-ERENDT-KEY
                                          PI-PREV-ERENDT-KEY


           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT

           IF RESP-NORMAL
              AND (PI-ELCRTO-KEY (1:33) =
                    OC-CONTROL-PRIMARY (1:33))
              AND (OC-RECORD-TYPE = 'I')
      *        DISPLAY ' RESP NORMAL AND = '
              IF OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES
                 MOVE OC-CONTROL-PRIMARY  TO PI-ELCRTO-KEY
              ELSE
                 SUBTRACT +1 FROM OC-KEY-SEQ-NO
                 MOVE OC-KEY-SEQ-NO    TO PI-CRTO-SEQ-NO
                 PERFORM 0380-ADD-CRTO-REC THRU 0380-EXIT
              END-IF
           ELSE
              MOVE SPACES              TO ORIGINAL-CERTIFICATE
              MOVE 'OC'                TO OC-RECORD-ID
              MOVE ELCRTO-KEY (1:33)   TO OC-CONTROL-PRIMARY (1:33)
              MOVE +4096               TO OC-KEY-SEQ-NO
                                          PI-CRTO-SEQ-NO
              PERFORM 0380-ADD-CRTO-REC THRU 0380-EXIT
           END-IF

           PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT

           .
       0360-EXIT.
           EXIT.

       0361-MOVE-CRTO-TO-SCREEN.

      *     display ' made it to 0361 '
           
           MOVE LOW-VALUES               TO EL631MI
           MOVE OC-LF-BENCD              TO WS-LF-BENCD
           MOVE OC-AH-BENCD              TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE OC-INS-FIRST-NAME        TO OIFNAMEO
           MOVE OC-INS-MIDDLE-INIT       TO OIMIDO
           MOVE OC-INS-LAST-NAME         TO OILNAMEO
           MOVE OC-INS-AGE               TO OIAGEO
           IF OC-INS-AGE-DEFAULT-FLAG = 'Y'
               MOVE '*'                  TO OIAGEDFO
           ELSE
               MOVE ' '                  TO OIAGEDFO
           END-IF
           MOVE OC-JNT-FIRST-NAME        TO OJFNAMEO
           MOVE OC-JNT-MIDDLE-INIT       TO OJMIDO
           MOVE OC-JNT-LAST-NAME         TO OJLNAMEO
           MOVE OC-JNT-AGE               TO OJAGEO
           IF OC-JNT-AGE-DEFAULT-FLAG = 'Y'
               MOVE '*'                  TO OJAGEDFO
           ELSE
               MOVE ' '                  TO OJAGEDFO
           END-IF
           MOVE OC-1ST-PMT-DT            TO DC-BIN-DATE-1
           MOVE ' '                      TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT   TO O1PMTDTO
           END-IF
           MOVE OC-CRED-BENE-NAME        TO OBENEO
           IF OC-LF-BENCD NOT = '00' AND '  '
              MOVE OC-LF-BENCD           TO OLBENCDO
              MOVE OC-LF-BEN-AMT         TO OLBENAO
              MOVE OC-LF-PRM-AMT         TO OLPREMO
              MOVE OC-LF-ALT-BEN-AMT     TO OLALTBNO
              MOVE OC-LF-ALT-PRM-AMT     TO OLALTPRO
              MOVE OC-LF-TERM            TO OLTERMO
              MOVE WS-LF-ABBRV           TO OLCOVTO
              MOVE OC-LF-EXP-DT          TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO OLEXPDTO
              END-IF
           END-IF

           IF OC-AH-BENCD NOT = '00' AND '  '
              MOVE OC-AH-BENCD           TO OABENCDO
              MOVE OC-AH-BEN-AMT         TO OABENAO
              MOVE OC-AH-PRM-AMT         TO OAPREMO
              MOVE OC-AH-CP              TO OCPO
              MOVE OC-AH-TERM            TO OATERMO
              MOVE WS-WAIT-PER           TO OWAITPO
              MOVE WS-AH-DESC            TO OACOVTO
              MOVE OC-AH-EXP-DT          TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO OAEXPDTO
              END-IF
           END-IF

           .
       0361-EXIT.
           EXIT.

       0362-READ-ELCRTO.

      *     display ' made it to 0362 '
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0362-EXIT.
           EXIT.


       0363-READ-ELCRTO-UPD.
      
      *     display ' made it to 0363 '
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
               UPDATE
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
               DISPLAY 'ERROR IN 0363 READING ELCRTO ' WS-RESPONSE
               MOVE ER-9999      TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8200-SEND-DATAONLY
           END-IF
      
           .
       0363-EXIT.
           EXIT.
      

       0365-READ-ERENDT.
      *     display 'made it to 0365'
           EXEC CICS READ
               DATASET   (WS-ERENDT-FILE-ID)
               SET       (ADDRESS OF ENDORSEMENT-RECORD)
               RIDFLD    (ERENDT-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0365-EXIT.
           EXIT.


       0370-MOVE-CERT-TO-SCREEN.
      *      display 'made it to 0370'

           MOVE WS-ELCERT-KEY         TO WS-ELCRTT-PRIMARY
           MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
      
           EXEC CICS READ
                DATASET  (WS-ELCRTT-FILE-ID)
                RIDFLD   (WS-ELCRTT-KEY)
                SET      (ADDRESS OF CERTIFICATE-TRAILERS)
                RESP     (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
              MOVE SPACES TO CS-INS-AGE-DEFAULT-FLAG
                             CS-JNT-AGE-DEFAULT-FLAG 
           END-IF
      

           MOVE CM-CARRIER             TO CARRO
           MOVE CM-GROUPING            TO GROUPO
           MOVE CM-STATE               TO STATEO
           MOVE CM-ACCOUNT             TO ACCTO
           MOVE CM-CERT-PRIME          TO CERTNOO
           MOVE CM-CERT-SFX            TO CRTSFXO
       
           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF

           MOVE CM-LF-BENEFIT-CD       TO WS-LF-BENCD
           MOVE CM-AH-BENEFIT-CD       TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE CM-LOAN-1ST-PMT-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO N1PMTDTO 
           END-IF

           MOVE CM-INSURED-FIRST-NAME  TO NIFNAMEO
           MOVE CM-INSURED-INITIAL2    TO NIMIDO
           MOVE CM-INSURED-LAST-NAME   TO NILNAMEO
           MOVE CM-INSURED-ISSUE-AGE   TO NIAGEO
           IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
               MOVE '*'                TO NIAGEDFO
           ELSE
               MOVE ' '                TO NIAGEDFO
           END-IF
           MOVE CM-JT-FIRST-NAME       TO NJFNAMEO
           MOVE CM-JT-INITIAL          TO NJMIDO
           MOVE CM-JT-LAST-NAME        TO NJLNAMEO
           MOVE CM-INSURED-JOINT-AGE   TO NJAGEO
           IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
               MOVE '*'                TO NJAGEDFO
           ELSE
               MOVE ' '                TO NJAGEDFO
           END-IF

           IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
              MOVE CM-LF-BENEFIT-CD    TO NLBENCDO
              MOVE CM-LF-BENEFIT-AMT   TO NLBENAO
              MOVE CM-LF-PREMIUM-AMT   TO NLPREMO
              MOVE CM-LF-ALT-BENEFIT-AMT TO NLALTBNO
              MOVE CM-LF-ALT-PREMIUM-AMT TO NLALTPRO
              MOVE CM-LF-ORIG-TERM     TO NLTERMO
              MOVE WS-LF-ABBRV         TO NLCOVTO
              MOVE CM-LF-LOAN-EXPIRE-DT TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NLEXPDTO
              END-IF
           END-IF

           IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
              MOVE CM-AH-BENEFIT-CD      TO NABENCDO
              MOVE CM-AH-BENEFIT-AMT     TO NABENAO
              MOVE CM-AH-PREMIUM-AMT     TO NAPREMO
              MOVE CM-AH-ORIG-TERM       TO NATERMO
              MOVE CM-AH-CRITICAL-PERIOD TO NCPO
              MOVE WS-WAIT-PER           TO NWAITPO
              MOVE WS-AH-DESC            TO NACOVTO
              MOVE CM-AH-LOAN-EXPIRE-DT  TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO NAEXPDTO
              END-IF
           END-IF

           IF CM-LIFE-COMM-PCT NOT = ZEROS
              MOVE CM-LIFE-COMM-PCT    TO LFCOMMO
           END-IF
           IF CM-AH-COMM-PCT NOT = ZEROS
              MOVE CM-AH-COMM-PCT      TO AHCOMMO
           END-IF

           MOVE WS-ELCERT-KEY         TO WS-ERMAIL-KEY
      
           EXEC CICS READ
                DATASET   (WS-ERMAIL-FILE-ID)
                RIDFLD    (WS-ERMAIL-KEY)
                SET       (ADDRESS OF MAILING-DATA)
                RESP      (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
               MOVE MA-CRED-BENE-NAME TO NBENEO
           END-IF.

122313     IF (CM-LF-CANCEL-DT NOT EQUAL LOW-VALUES AND SPACES) AND
122313        CM-CREDIT-INTERFACE-SW-2 < '4'
122313          MOVE 'Y' TO PI-CERT-HAS-CANCEL
122313     END-IF
122313     IF (CM-AH-CANCEL-DT NOT EQUAL LOW-VALUES AND SPACES) AND
122313        CM-CREDIT-INTERFACE-SW-2 < '4'
122313          MOVE 'Y' TO PI-CERT-HAS-CANCEL
122313     END-IF
122313
           .
       0370-EXIT.
           EXIT.


       0375-RESET-CRTO-REC.
      *      display 'made it to 0375'
           PERFORM 0383-LOAD-CERT-TO-CRTO-REC THRU 0383-EXIT

           EXEC CICS REWRITE
              DATASET   ('ELCRTO')
              FROM      (ORIGINAL-CERTIFICATE)
              RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
               DISPLAY 'ERROR IN 0375 REWRITING ELCRTO ' WS-RESPONSE
               MOVE ER-9999      TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT

           .
       0375-EXIT.
           EXIT.

       0380-ADD-CRTO-REC.
      *      display 'made it to 0380'
           PERFORM 0383-LOAD-CERT-TO-CRTO-REC THRU 0383-EXIT

           EXEC CICS WRITE
              DATASET   ('ELCRTO')
              FROM      (ORIGINAL-CERTIFICATE)
              RIDFLD    (OC-CONTROL-PRIMARY)
              RESP      (WS-RESPONSE)
           END-EXEC.

           IF NOT RESP-NORMAL
               DISPLAY 'ERROR IN 0380 WRITING ELCRTO ' WS-RESPONSE
               MOVE ER-9999      TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0380-EXIT.
           EXIT.


       0383-LOAD-CERT-TO-CRTO-REC.
       
      *     display 'made it to 0383'

           MOVE '6314'                 TO OC-LAST-MAINT-BY
           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
           MOVE 'I'                    TO OC-RECORD-TYPE
           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME   
           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME  
           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT 
           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE         
           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME   
           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME  
           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT 
           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE         
           MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD        
           IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
               MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM         
               MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT      
               MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT      
               MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT  
               MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT  
               MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT       
               MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT     
               MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
               MOVE CM-LF-ITD-CANCEL-AMT TO OC-LF-ITD-CANCEL-AMT
           END-IF
           MOVE CM-AH-BENEFIT-CD       TO OC-AH-BENCD        
           IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
               MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM         
               MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT      
               MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT      
               MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT       
               MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT     
               MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
               MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
               MOVE CM-AH-ITD-CANCEL-AMT TO OC-AH-ITD-CANCEL-AMT
           END-IF
           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
           MOVE 'N'                    TO OC-ISSUE-TRAN-IND
           MOVE 'N'                    TO OC-CANCEL-TRAN-IND

           MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
      
           MOVE ELCRTO-KEY (1:33) TO WS-ERMAIL-KEY
      
           EXEC CICS READ
                DATASET   (WS-ERMAIL-FILE-ID)
                RIDFLD    (WS-ERMAIL-KEY)
                SET       (ADDRESS OF MAILING-DATA)
                RESP      (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
               MOVE MA-CRED-BENE-NAME TO OC-CRED-BENE-NAME
           END-IF.
      
           MOVE ELCRTO-KEY (1:33)     TO WS-ELCRTT-PRIMARY
           MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
      
           EXEC CICS READ
                DATASET  (WS-ELCRTT-FILE-ID)
                RIDFLD   (WS-ELCRTT-KEY)
                SET      (ADDRESS OF CERTIFICATE-TRAILERS)
                RESP     (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
               MOVE CS-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
               MOVE CS-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
           END-IF.
            

           .
       0383-EXIT.
           EXIT.


       0385-CHG-CRTO-REC.
       
      *     display 'made it to 0385'

           MOVE '6314'                 TO OC-LAST-MAINT-BY
           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
           MOVE 'I'                    TO OC-RECORD-TYPE
           MOVE SPACES                 TO OC-ORIG-REC
           MOVE OILNAMEO               TO OC-INS-LAST-NAME   
           MOVE OIFNAMEO               TO OC-INS-FIRST-NAME  
           MOVE OIMIDO                 TO OC-INS-MIDDLE-INIT 
           MOVE OIAGEO                 TO OC-INS-AGE         
           MOVE OJLNAMEO               TO OC-JNT-LAST-NAME   
           MOVE OJFNAMEO               TO OC-JNT-FIRST-NAME  
           MOVE OJMIDO                 TO OC-JNT-MIDDLE-INIT 
           MOVE OJAGEO                 TO OC-JNT-AGE
           MOVE SPACES                 TO OC-LF-BENCD
           MOVE ZEROS                  TO OC-LF-TERM         
                                          OC-LF-BEN-AMT      
                                          OC-LF-PRM-AMT      
                                          OC-LF-ALT-BEN-AMT
                                          OC-LF-ALT-PRM-AMT
                                          OC-LF-CANCEL-AMT
                                          OC-LF-ITD-CANCEL-AMT
           MOVE LOW-VALUES             TO OC-AH-EXP-DT
           IF OLBENCDO NOT EQUAL '00' AND SPACES AND LOW-VALUES
              MOVE OLBENCDO            TO OC-LF-BENCD
              MOVE OLTERMO             TO OC-LF-TERM
              MOVE OLBENAO             TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-LF-BEN-AMT
              MOVE OLPREMO             TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-LF-PRM-AMT
              MOVE OLALTBNO            TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-LF-ALT-BEN-AMT
              MOVE OLALTPRO            TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-LF-ALT-PRM-AMT
              MOVE OLEXPDTO            TO WS-TMPDATE
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-TMPDATE)
                   LENGTH  (8)
              END-EXEC
              MOVE WS-TMPDT            TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NOT DATE-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO OC-LF-EXP-DT
              END-IF
           END-IF
           MOVE SPACES                 TO OC-AH-BENCD
           MOVE ZEROS                  TO OC-AH-TERM         
                                          OC-AH-BEN-AMT      
                                          OC-AH-PRM-AMT      
                                          OC-AH-CANCEL-AMT
                                          OC-AH-CP
           MOVE LOW-VALUES             TO OC-AH-EXP-DT
           IF OABENCDO NOT EQUAL '00' AND SPACES AND LOW-VALUES
              MOVE OABENCDO            TO OC-AH-BENCD
              MOVE OATERMO             TO OC-AH-TERM         
              MOVE OABENAO             TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-AH-BEN-AMT
              MOVE OAPREMO             TO WS-AMTX
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-AMTX)
                   LENGTH  (12)
              END-EXEC  
              MOVE WS-AMTV2            TO OC-AH-PRM-AMT
              MOVE OAEXPDTO            TO WS-TMPDATE
              EXEC CICS BIF DEEDIT
                   FIELD   (WS-TMPDATE)
                   LENGTH  (8)
              END-EXEC
              MOVE WS-TMPDT            TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NOT DATE-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO OC-AH-EXP-DT
              END-IF           
              MOVE OCPO                TO OC-AH-CP
           END-IF
           MOVE LFCOMMO                TO OC-LF-COMM-PCT
           MOVE AHCOMMO                TO OC-AH-COMM-PCT
           MOVE O1PMTDTO               TO WS-TMPDATE
           EXEC CICS BIF DEEDIT
                FIELD   (WS-TMPDATE)
                LENGTH  (8)
           END-EXEC
           MOVE WS-TMPDT               TO DC-GREG-DATE-1-MDY
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NOT DATE-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO OC-1ST-PMT-DT
           END-IF           
           MOVE 'N'                    TO OC-ISSUE-TRAN-IND
           MOVE 'N'                    TO OC-CANCEL-TRAN-IND

           MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
      
           MOVE ELCRTO-KEY (1:33) TO WS-ERMAIL-KEY
      
           EXEC CICS READ
                DATASET   (WS-ERMAIL-FILE-ID)
                RIDFLD    (WS-ERMAIL-KEY)
                SET       (ADDRESS OF MAILING-DATA)
                RESP      (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
               MOVE MA-CRED-BENE-NAME TO OC-CRED-BENE-NAME
           END-IF.
      
           MOVE ELCRTO-KEY (1:33)     TO WS-ELCRTT-PRIMARY
           MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
      
           EXEC CICS READ
                DATASET  (WS-ELCRTT-FILE-ID)
                RIDFLD   (WS-ELCRTT-KEY)
                SET      (ADDRESS OF CERTIFICATE-TRAILERS)
                RESP     (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
               MOVE CS-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
               MOVE CS-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
           END-IF.

           EXEC CICS REWRITE
              DATASET   ('ELCRTO')
              FROM      (ORIGINAL-CERTIFICATE)
              RESP      (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
               DISPLAY 'ERROR IN 0385 REWRITING ELCRTO ' WS-RESPONSE
               MOVE ER-9999      TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 0361-MOVE-CRTO-TO-SCREEN THRU 0361-EXIT

           .
       0385-EXIT.
           EXIT.


       0390-BUILD-FROM-ERENDT.
      *     display 'made it to 0390'
           MOVE LOW-VALUES             TO EL631MI
           MOVE PI-COMPANY-CD          TO PI-ENDT-COMPANY-CD
           MOVE EN-CARRIER             TO PI-ENDT-CARRIER
           MOVE EN-GROUPING            TO PI-ENDT-GROUPING
           MOVE EN-STATE               TO PI-ENDT-STATE
           MOVE EN-ACCOUNT             TO PI-ENDT-ACCOUNT
           MOVE EN-CERT-PRIME          TO PI-ENDT-CERT-PRIME
           MOVE EN-CERT-SFX            TO PI-ENDT-CERT-SFX
           MOVE EN-CERT-EFF-DT         TO PI-ENDT-CERT-EFF-DT
           MOVE EN-REC-TYPE            TO PI-ENDT-RECORD-TYPE
           MOVE EN-SEQ-NO              TO PI-ENDT-SEQ-NO
           MOVE PI-ERENDT-KEY          TO PI-PREV-ERENDT-KEY

           MOVE EN-CARRIER             TO CARRO
           MOVE EN-GROUPING            TO GROUPO
           MOVE EN-STATE               TO STATEO
           MOVE EN-ACCOUNT             TO ACCTO
           MOVE EN-CERT-PRIME          TO CERTNOO
           MOVE EN-CERT-SFX            TO CRTSFXO
           MOVE EN-SEQ-NO              TO SEQNOO
           MOVE EN-ARCHIVE-NO          TO ARCHNOO

           MOVE EN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF

           IF EN-LAST-MAINT-HHMMSS NOT NUMERIC
              MOVE +0                  TO EN-LAST-MAINT-HHMMSS
           END-IF

           MOVE EN-LAST-MAINT-HHMMSS   TO TIME-MT
                                          PI-UPDATE-HHMMSS

           IF EN-PROCESS-DT NOT = SPACES AND LOW-VALUES
              MOVE EN-PROCESS-DT       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO PROCDTO
              END-IF
           END-IF
           IF EN-TEMPLATE-USED NOT = SPACES
              MOVE EN-TEMPLATE-USED    TO TEMPLO
           END-IF

           IF EN-INPUT-DT NOT = SPACES AND LOW-VALUES
              MOVE EN-INPUT-DT         TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO INPDTO
              END-IF
           END-IF

           MOVE EN-LF-ORIG-BENCD       TO WS-LF-BENCD
           MOVE EN-AH-ORIG-BENCD       TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           MOVE EN-INS-ORIG-LAST-NAME    TO OILNAMEO          
           MOVE EN-INS-ORIG-FIRST-NAME   TO OIFNAMEO          
           MOVE EN-INS-ORIG-MIDDLE-INIT  TO OIMIDO
           MOVE EN-INS-ORIG-AGE          TO OIAGEO
           IF EN-INS-ORIG-AGE-DEF-FLAG = 'Y'
               MOVE '*'                  TO OIAGEDFO
           ELSE
               MOVE ' '                  TO OIAGEDFO
           END-IF
           MOVE EN-JNT-ORIG-LAST-NAME    TO OJLNAMEO          
           MOVE EN-JNT-ORIG-FIRST-NAME   TO OJFNAMEO          
           MOVE EN-JNT-ORIG-MIDDLE-INIT  TO OJMIDO
           MOVE EN-JNT-ORIG-AGE          TO OJAGEO
           IF EN-JNT-ORIG-AGE-DEF-FLAG = 'Y'
               MOVE '*'                  TO OJAGEDFO
           ELSE
               MOVE ' '                  TO OJAGEDFO
           END-IF

           IF EN-LF-ORIG-BENCD NOT = '00' AND '  '
              MOVE EN-LF-ORIG-BENCD    TO OLBENCDO
              MOVE EN-LF-ORIG-TERM     TO OLTERMO
              MOVE EN-LF-ORIG-BEN-AMT  TO OLBENAO
              MOVE EN-LF-ORIG-PRM-AMT  TO OLPREMO
              MOVE EN-LF-ORIG-ALT-BEN-AMT TO OLALTBNO
              MOVE EN-LF-ORIG-ALT-PRM-AMT TO OLALTPRO
              MOVE WS-LF-ABBRV         TO OLCOVTO
              MOVE EN-LF-ORIG-EXP-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO OLEXPDTO
              END-IF
           END-IF

           IF EN-AH-ORIG-BENCD NOT = '00' AND '  '
              MOVE EN-AH-ORIG-BENCD    TO OABENCDO
              MOVE EN-AH-ORIG-TERM     TO OATERMO
              MOVE EN-AH-ORIG-BEN-AMT  TO OABENAO
              MOVE EN-AH-ORIG-PRM-AMT  TO OAPREMO
              MOVE EN-AH-ORIG-CP       TO OCPO
              MOVE WS-WAIT-PER         TO OWAITPO
              MOVE WS-AH-DESC          TO OACOVTO
              MOVE EN-AH-ORIG-EXP-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO OAEXPDTO
              END-IF
           END-IF

           MOVE EN-ORIG-CRED-BENE      TO OBENEO

           MOVE EN-INS-NEW-LAST-NAME     TO NILNAMEO          
           MOVE EN-INS-NEW-FIRST-NAME    TO NIFNAMEO          
           MOVE EN-INS-NEW-MIDDLE-INIT   TO NIMIDO
           MOVE EN-INS-NEW-AGE       TO NIAGEO
           IF EN-INS-NEW-AGE-DEF-FLAG = 'Y'
               MOVE '*'                  TO NIAGEDFO
           ELSE
               MOVE ' '                  TO NIAGEDFO
           END-IF
           MOVE EN-JNT-NEW-LAST-NAME     TO NJLNAMEO          
           MOVE EN-JNT-NEW-FIRST-NAME    TO NJFNAMEO          
           MOVE EN-JNT-NEW-MIDDLE-INIT   TO NJMIDO
           MOVE EN-JNT-NEW-AGE           TO NJAGEO
           IF EN-JNT-NEW-AGE-DEF-FLAG = 'Y'
               MOVE '*'                  TO NJAGEDFO
           ELSE
               MOVE ' '                  TO NJAGEDFO
           END-IF
           MOVE EN-LF-NEW-BENCD        TO WS-LF-BENCD
           MOVE EN-AH-NEW-BENCD        TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT

           IF EN-LF-NEW-BENCD NOT = '00' AND '  '
              MOVE EN-LF-NEW-BENCD     TO NLBENCDO
              MOVE EN-LF-NEW-TERM      TO NLTERMO
              MOVE EN-LF-NEW-BEN-AMT   TO NLBENAO
              MOVE EN-LF-NEW-PRM-AMT   TO NLPREMO
              MOVE EN-LF-NEW-ALT-BEN-AMT TO NLALTBNO
              MOVE EN-LF-NEW-ALT-PRM-AMT TO NLALTPRO
              MOVE WS-LF-ABBRV         TO NLCOVTO
              MOVE EN-LF-NEW-EXP-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO NLEXPDTO
              END-IF
           END-IF

           IF EN-AH-NEW-BENCD NOT = '00' AND '  '
              MOVE EN-AH-NEW-BENCD     TO NABENCDO
              MOVE EN-AH-NEW-TERM      TO NATERMO
              MOVE EN-AH-NEW-BEN-AMT   TO NABENAO
              MOVE EN-AH-NEW-PRM-AMT   TO NAPREMO
              MOVE EN-AH-NEW-CP        TO NCPO
              MOVE WS-WAIT-PER         TO NWAITPO
              MOVE WS-AH-DESC          TO NACOVTO
              MOVE EN-AH-NEW-EXP-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO NAEXPDTO
              END-IF
           END-IF

           MOVE EN-NEW-CRED-BENE       TO NBENEO
           
           STRING EN-REASON-CODE (1)    ' '
                  EN-REASON-CODE (2)    ' '
                  EN-REASON-CODE (3)    ' '
                  EN-REASON-CODE (4)    ' '
                  EN-REASON-CODE (5)    ' '
                  EN-REASON-CODE (6)    ' '
                  EN-REASON-CODE (7)    ' '
                  EN-REASON-CODE (8)    ' '
                  EN-REASON-CODE (9)    ' '
                  EN-REASON-CODE (10)   ' '
                  EN-REASON-CODE (11)   ' '
                  EN-REASON-CODE (12)   ' '
              DELIMITED BY '  ' INTO REACDSO
           END-STRING
           MOVE EN-SIG-SW              TO SIGO
           MOVE EN-MONEY-SW            TO MONEYO
           MOVE EN-HEALTH-APP          TO HLTHAPPO
           MOVE EN-VOUCHER-SW          TO VCHERO
           MOVE EN-PAYEE               TO PAYEEO

           .
       0390-EXIT.
           EXIT.


       0400-COMPARE-FROM-TO.
      *      display 'made it to 0400'
            IF OILNAMEO = NILNAMEO AND
               OIFNAMEO = NIFNAMEO AND
               OIMIDO   = NIMIDO   AND
               OIAGEO   = NIAGEO   AND
               OJLNAMEO = NJLNAMEO AND
               OJFNAMEO = NJFNAMEO AND
               OJMIDO   = NJMIDO   AND
               OJAGEO   = NJAGEO   AND
               OLBENCDO = NLBENCDO AND   
               OLTERMO  = NLTERMO  AND   
               OLBENAO  = NLBENAO  AND   
               OLPREMO  = NLPREMO  AND 
               OLALTBNO = NLALTBNO AND 
               OLALTPRO = NLALTPRO AND   
               OLCOVTO  = NLCOVTO  AND
               OLEXPDTO = NLEXPDTO AND
               OABENCDO = NABENCDO AND                     
               OATERMO  = NATERMO  AND                     
               OABENAO  = NABENAO  AND                     
               OAPREMO  = NAPREMO  AND                     
               OCPO     = NCPO     AND                     
               OWAITPO  = NWAITPO  AND                     
               OACOVTO  = NACOVTO  AND                     
               OAEXPDTO = NAEXPDTO AND
               O1PMTDTO = N1PMTDTO
                  MOVE 'N'   TO PI-CHANGES-MADE
            ELSE
                  MOVE 'Y'   TO PI-CHANGES-MADE
            END-IF
      
            IF OLBENCDO = NLBENCDO AND   
               OLTERMO  = NLTERMO  AND   
               OLBENAO  = NLBENAO  AND   
               OLPREMO  = NLPREMO  AND 
               OLALTBNO = NLALTBNO AND 
               OLALTPRO = NLALTPRO AND   
               OLCOVTO  = NLCOVTO  AND
               OLEXPDTO = NLEXPDTO AND
               OABENCDO = NABENCDO AND                     
               OATERMO  = NATERMO  AND                     
               OABENAO  = NABENAO  AND                     
               OAPREMO  = NAPREMO  AND                     
               OCPO     = NCPO     AND                     
               OWAITPO  = NWAITPO  AND                     
               OACOVTO  = NACOVTO  AND                     
               OAEXPDTO = NAEXPDTO AND
               O1PMTDTO = N1PMTDTO
                  MOVE 'N'   TO PI-AMT-CHANGES-MADE
            ELSE
                  MOVE 'Y'   TO PI-AMT-CHANGES-MADE
            END-IF
      
            MOVE SPACES TO WS-REASON-CODE-IND
      
            IF OILNAMEO NOT = NILNAMEO OR
               OIFNAMEO NOT = NIFNAMEO OR
               OIMIDO   NOT = NIMIDO  
                MOVE 'A' TO WS-REASON-A
            END-IF
      
            IF OJLNAMEO NOT = NJLNAMEO OR
               OJFNAMEO NOT = NJFNAMEO OR
               OJMIDO   NOT = NJMIDO  
                MOVE 'B' TO WS-REASON-B
            END-IF
      
            IF OIAGEO NOT = NIAGEO     
                MOVE 'C' TO WS-REASON-C
            END-IF
      
            IF OJAGEO NOT = NJAGEO     
                MOVE 'D' TO WS-REASON-D
            END-IF
      
            IF OLBENCDO NOT = NLBENCDO
                MOVE 'E' TO WS-REASON-E
            END-IF
      
            IF OLBENAO NOT = NLBENAO  OR   
               OLALTBNO NOT = NLALTBNO
                MOVE 'F' TO WS-REASON-F
            END-IF
      
            IF OLPREMO NOT = NLPREMO  OR 
               OLALTPRO NOT = NLALTPRO 
                MOVE 'G' TO WS-REASON-G
            END-IF
      
            IF OLTERMO NOT = NLTERMO     
                MOVE 'H' TO WS-REASON-H
            END-IF
      
            IF OLEXPDTO NOT = NLEXPDTO 
                MOVE 'I' TO WS-REASON-I
            END-IF
      
            IF OABENCDO NOT = NABENCDO
                MOVE 'J' TO WS-REASON-J
            END-IF
      
            IF OABENAO NOT = NABENAO
                MOVE 'K' TO WS-REASON-K
            END-IF
      
            IF OAPREMO NOT = NAPREMO
                MOVE 'L' TO WS-REASON-L
            END-IF
      
            IF OATERMO NOT = NATERMO
                MOVE 'M' TO WS-REASON-M
            END-IF
      
            IF OAEXPDTO NOT = NAEXPDTO
                MOVE 'N' TO WS-REASON-N
            END-IF
      
            MOVE WS-REASON-CODE-IND TO PI-6318-REASON-IND
      
           .
       0400-EXIT.
           EXIT.
      

       0510-ERENDT-READNEXT.

           EXEC CICS READNEXT
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              SET        (ADDRESS OF ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2237             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0510-EXIT.
           EXIT.

       0520-ERENDT-STARTBR.

           EXEC CICS STARTBR
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2237             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO CARRL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0520-EXIT.
           EXIT.

       0530-ERENDT-READPREV.

           EXEC CICS READPREV
              DATASET    (WS-ERENDT-FILE-ID)
              RIDFLD     (ERENDT-KEY)
              SET        (ADDRESS OF ENDORSEMENT-RECORD)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
122313        MOVE ER-2238             TO EMI-ERROR
122313        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122313        MOVE 'N'              TO PI-ENDORSE-RECORD
122313        MOVE LOW-VALUES       TO  EL631MI
              GO TO 0350-DISPLAY-COMMON-DATA
           END-IF

           .
       0530-EXIT.
           EXIT.

       1130-MATCH-BENCDS.

           MOVE SPACES                 TO WS-LF-ABBRV
                                          WS-LF-DESC
                                          WS-WAIT-PER
                                          WS-RET-ELIM
                                          WS-AH-DESC
                                          WS-BEN-DAYS
           IF WS-LF-BENCD NOT = ZEROS AND SPACES
              PERFORM 1140-MATCH-LFBEN THRU 1140-EXIT
           END-IF

           IF WS-AH-BENCD NOT = ZEROS AND SPACES
              PERFORM 1150-MATCH-AHBEN THRU 1150-EXIT
           END-IF

           .
       1130-EXIT.
           EXIT.

       1140-MATCH-LFBEN.

           MOVE PI-COMPANY-ID          TO ELCNTL-KEY
           MOVE '4'                    TO CNTL-REC-TYPE
           MOVE WS-LF-BENCD            TO CNTL-BEN-CD
           MOVE +0                     TO CNTL-SEQ
           EXEC CICS READ
              SET     (ADDRESS OF CONTROL-FILE)
              DATASET (WS-ELCNTL-FILE-ID)
              RIDFLD  (ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-LF-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-DESCRIP (S1)
                                       TO WS-LF-ABBRV
                 MOVE WS-LF-BENCD      TO WS-LF-DESC
              ELSE
                 MOVE 'XX'             TO WS-LF-DESC
                 MOVE 'XXX'            TO WS-LF-ABBRV
              END-IF
           ELSE
              DISPLAY ' NO LF BEN CD ' WS-LF-BENCD
           END-IF

           .
       1140-EXIT.
           EXIT.

       1150-MATCH-AHBEN.

           MOVE PI-COMPANY-ID          TO ELCNTL-KEY
           MOVE '5'                    TO CNTL-REC-TYPE
           MOVE WS-AH-BENCD            TO CNTL-BEN-CD
           MOVE +0                     TO CNTL-SEQ
           EXEC CICS READ
              SET     (ADDRESS OF CONTROL-FILE)
              DATASET (WS-ELCNTL-FILE-ID)
              RIDFLD  (ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-AH-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-ALPHA (S1)
                                       TO WS-WAIT-PER
                 MOVE CF-BENEFIT-ALPHA (S1) (3:1)
                                       TO WS-RET-ELIM
                 MOVE CF-BENEFIT-ALPHA (S1) (1:2)
                                       TO WS-BEN-DAYS
                 IF CF-SPECIAL-CALC-CD (S1) = 'C'
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'JOINT CRITICAL PERIOD DISABILITY'
                                       TO WS-AH-DESC
                    ELSE
                       MOVE 'SINGLE CRITICAL PERIOD DISABILITY'
                                       TO WS-AH-DESC
                    END-IF
                 ELSE
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'JOINT DISABILITY'
                                       TO WS-AH-DESC
                    ELSE
                       MOVE 'SINGLE DISABILITY'
                                       TO WS-AH-DESC
                    END-IF
                 END-IF
              ELSE
                 MOVE 'NOT FOUND'      TO WS-AH-DESC
              END-IF
           ELSE
              DISPLAY ' NO AH BEN CD ' WS-AH-BENCD
           END-IF

           .
       1150-EXIT.
           EXIT.



       8100-SEND-INITIAL-MAP.

           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
           MOVE PI-COMPANY-ID          TO COMPANYO
           MOVE '2'                    TO DATASORO
           MOVE PI-PROCESSOR-ID        TO PROCIDO
           
           IF PI-PROCESSOR-IS-CSR-SUPER
122313      AND PI-ENDORSE-RECORD NOT EQUAL 'Y'
               MOVE  AL-UANON          TO OILNAMEA
                                          OIFNAMEA
                                          OIMIDA
                                          OJLNAMEA   
                                          OJFNAMEA
                                          OJMIDA
                                          OLBENCDA
                                          OABENCDA
               MOVE AL-UNNON           TO OIAGEA
                                          OJAGEA
                                          OLTERMA
                                          OLBENAA
                                          OLPREMA
                                          OLALTBNA
                                          OLALTPRA
                                          OLEXPDTA
                                          OATERMA
                                          OABENAA 
                                          OAPREMA 
                                          OCPA 
                                          OWAITPA
                                          OAEXPDTA
                                          O1PMTDTA
               MOVE AL-PANON           TO LFCOMMA
                                          AHCOMMA
                                          OLCOVTA
                                          OACOVTA
               MOVE AL-SANOF           TO SUPPF6A
                                          SUPPF4A
           END-IF
122313
122313     IF PI-ENDORSE-RECORD NOT EQUAL 'Y'           
122313         MOVE AL-SANOF           TO VERPF3A
122313     END-IF
122313
122313     IF PI-CERT-HAS-CANCEL = 'Y'
122313         MOVE AL-SANOF           TO CANPF7A
122313     END-IF
                                            
      *    MOVE ZEROS                  TO ARCHNOO

           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO CARRL
           IF EMI-ERROR-NUMBER (1) = ER-2883
               MOVE 'W'                TO EMI-SEVERITY (1)
           END-IF.
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.

           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631MO)
               ERASE
               CURSOR
           END-EXEC.

           GO TO 9100-RETURN-TRAN.

       8200-SEND-DATAONLY.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO PFENTRL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631MO)
               DATAONLY
               ERASEAUP
               CURSOR
           END-EXEC.

           GO TO 9100-RETURN-TRAN.

       8300-SEND-TEXT.
           EXEC CICS SEND TEXT
               FROM     (LOGOFF-TEXT)
               LENGTH   (LOGOFF-LENGTH)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.

       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       9000-RETURN-CICS.
           EXEC CICS RETURN
           END-EXEC.

       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.

           EXEC CICS RETURN
               TRANSID    (TRANS-ID)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.

       9300-XCTL.
           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.

           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (1300)
           END-EXEC.

       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.

       9600-PGMID-ERROR.
           EXEC CICS HANDLE CONDITION
               PGMIDERR    (8300-SEND-TEXT)
           END-EXEC.

           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.

       9700-DATE-LINK.
           MOVE LINK-ELDATCV           TO PGM-NAME.

           EXEC CICS LINK
               PROGRAM    (PGM-NAME)
               COMMAREA   (DATE-CONVERSION-DATA)
               LENGTH     (DC-COMM-LENGTH)
           END-EXEC.

       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
              MOVE LINK-001            TO PGM-NAME
              EXEC CICS LINK
                  PROGRAM    (PGM-NAME)
                  COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
                  LENGTH     (EMI-COMM-LENGTH)
              END-EXEC
           END-IF

           .
       9900-EXIT.
           EXIT.

       9990-ABEND.
           MOVE LINK-004               TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           EXEC CICS LINK
               PROGRAM   (PGM-NAME)
               COMMAREA  (EMI-LINE1)
               LENGTH    (72)
           END-EXEC.

           GO TO 8200-SEND-DATAONLY.

           GOBACK.

       9995-SECURITY-VIOLATION.
                                   COPY ELCSCTP.

       9995-EXIT.
           EXIT.

