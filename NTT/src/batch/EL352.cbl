       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL352.
      * THIS PROGRAM IS A STRIPPED DOWN VERSION OF EL176.
      *
      *AUTHOR.    DOUG NELSON.

      *REMARKS.
      *        THIS PROGRAM READS THE CHECK QUEUE FILE AND SELECTS
      *    CHECKS TO PRINT AND WRITES THEM TO THE MICR DRAFT FILE.

      *    INPUT FILES - SYS019 - DATE CARD
      *                  SYS006 - CONTROL CARD FILE WITH PROCESSING OPTIONS.
      *                  ELCHKQ2 - CHECK QUEUE
      *                  ELTRLR - ACTIVITY TRAILERS
      *                  ELCNTL - CONTROL FILE
      *                  ELMSTR - CLAIM MASTER
      *                  ELCERT - CERTIFICATE MASTER
      *                  ELBENE - BENEFICIARY MASTER
      *                  ERACCT - ACCOUNT MASTER

      *    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS
      *                   ELCHKQ2 - CHECK QUEUE
      *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 040100                   DJNA  DRAFT NUMBER EXPANSION.
      * 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
      * 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
      * 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
      * 042704    2004042700002  PEMA  REMOVE NOTIFY NAME AND ADDRESS
      * 070104    IR             PEMA  MODIFY LENGTH OF MICRDRFT
      * 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
      * 032805    2005011100005  DJNA  CONVERT ONLINE EL176 TO BATCH EL352
      * 080305  IR2005070600001  PEMA  MODIFY PER REQUEST  
112805* 112805    2005033100001  PEMA  ADD PROCESSING FOR FORM DCC2
030906* 030906  IR2006030600002  AJRA  CHG MESSAGE FOR PAST PARTIAL PMT
032106* 032106                   AJRA  PUT FULL DATE IN 420C MSG 1
041706* 041706                   PEMA  CHANGE COMP NAME ON DCC CAR 3,4
041806* 041806                   PEMA  CHANGE ADDRESS AGAIN
062006* 062006    2006050100003  PEMA  ADD CSO FAMILY CO COMPANIES
113006* 113006  CR2006111300003  PEMA  ADD PROCESSING FOR KY
092507* 092507                   AJRA  NOTICE TO BORROWER TOO BIG-USE NOTE 
090108* 090108    2007041300006  AJRA  REMOVE DRAFT MESSAGES FROM AUTO PAY
090108* 090108                         SET PAYMENT TYPE TO A FOR AUTO PAY
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FOR AK
101708* 101708    2008050500001  AJRA  ADD PROCESSING FOR FORM DCC4 (CCC)
042809* 042809    2009031600001  AJRA  ADD BIU MESSAGE WITH VIN FROM CERT TRL
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
072110* 072110    2009122800001  AJRA  NAPERSOFT EXTRACT
091310* 091310    2009122800001  AJRA  CHG ENCLOSURE CODE ON FINAL PMT
092010* 092010    2009122800001  AJRA  MISC NAPERSOFT CHANGES
012011* 012011    2009122800001  AJRA  FIX DCC IU ENC CODE
030311* 030311		2011021000002  AJRA  MODIFY BAR CODE FOR SURVEY
032311* 032311    2011010400001  AJRA  ADD ELIM EOB NOTE
032311*                                FIX 8700-LOCATE-BENEFIT
061311* 061311    2011050300002  AJRA  ADD DFTE LETTER FOR NON B AH PAYEES
062711* 062711    2011062200003  AJRA  EXC PMTS BEYOND CURR PD THRU IN EOB HIST 
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
031612* 031612  CR2011120900003  AJRA  ADD AHL CLAIM NUMBER
040312* 040312  IR2012040300004  PEMA  GO TO EOJ IF NO CHECKS TO PRINT
013013* 013013    2012092400003  AJRA  SPECIAL EOB FOR ACCELERATED DEATH
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM IND
041813* 041813    2013040400003  AJRA  USE X ENC CODES ON SPECIAL RELEASE CHKS
052813* 052813    2013052400001  AJRA  PASS ENTERED PAYEE ADDRESS FOR INSURED
102413* 102413    2013100800001  AJRA  CHECK FOR SPECIAL RELEASE INDICATOR
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
052218* 052218  CR2015012900002  PEMA  Use CC date instead of CPU date
082218* 082218  CR2018051400001  TANA  Hold and Pay
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
082219* 082219  CR2019011400002  PEMA  Add account_name column to table.
071420* 071420  IR2020071400001  PEMA  Correct/Add VPP processing
090120* 090120  IR2020083100001  PEMA  Correct VPP processing
062821* 063021  CR2021021600001  TANA  ADD FNL COMPANY CODE
100621* 100621  IR2021093000002  PEMA  Remove drft seq from ach pmts
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************
           EJECT
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DISK-DATE       ASSIGN TO SYS019.

           SELECT CONTROL-CARD    ASSIGN TO SYS006.

           SELECT MICR-DRAFT-FILE
                               ASSIGN TO SYS010-3380-MICRDRFT
                                ORGANIZATION IS INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY IS MICR-KEY
                                FILE STATUS IS MICR-STATUS.

           SELECT ELCHKQ2      ASSIGN TO SYS011-3380-ELCHKQ2
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CQ-CONTROL-BY-PAYEE
                                                IN ELCHKQ2
                                FILE STATUS     CHKQ-STATUS.

           SELECT ELCNTL       ASSIGN TO SYS012-3380-ELCNTL
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CF-CONTROL-PRIMARY
                                                IN ELCNTL
                                FILE STATUS     CNTL-STATUS.

           SELECT ELTRLR       ASSIGN TO SYS013-3380-ELTRLR
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      AT-CONTROL-PRIMARY
                                                IN ELTRLR
                                FILE STATUS     TRLR-STATUS.

           SELECT ELCERT       ASSIGN TO SYS014-3380-ELCERT
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CM-CONTROL-PRIMARY
                                                IN ELCERT
                                FILE STATUS     CERT-STATUS.

           SELECT ELMSTR       ASSIGN TO SYS015-3380-ELMSTR
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      CL-CONTROL-PRIMARY
                                                IN ELMSTR
                                FILE STATUS     MSTR-STATUS.

           SELECT ERACCT       ASSIGN TO SYS016-3380-ERACCT
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      AM-CONTROL-PRIMARY
                                                IN ERACCT
                                FILE STATUS     ACCT-STATUS.

           SELECT ELBENE       ASSIGN TO SYS017-3380-ELBENE
                                ORGANIZATION    INDEXED
                                ACCESS          DYNAMIC
                                RECORD KEY      BE-CONTROL-PRIMARY
                                                IN ELBENE
                                FILE STATUS     BENE-STATUS.

042809
042809     SELECT ELCRTT           ASSIGN TO             ELCRTT         
042809                             ORGANIZATION IS INDEXED              
042809                             ACCESS IS DYNAMIC                    
042809                             RECORD KEY IS CS-CONTROL-PRIMARY     
042809                             FILE STATUS IS ELCRTT-FILE-STATUS.   

072110     SELECT NAPERSOFT-LETTERS ASSIGN TO SYS020
072110                              ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                         COPY ELCDTEFD.

       FD  CONTROL-CARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  CARD-IN-AREA            PIC X(80).

       FD  MICR-DRAFT-FILE.
       01  MICR-RECORD.
           COPY MICR420C.

       FD  ELCHKQ2.

           COPY ELCCHKQ.

       FD  ELCNTL.

           COPY ELCCNTL.

       FD  ELTRLR.

           COPY ELCTRLR.

       FD  ELCERT.

           COPY ELCCERT.

       FD  ELMSTR.

           COPY ELCMSTR.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ELBENE.

           COPY ELCBENE.
           
042809 FD  ELCRTT.
042809                                 COPY ELCCRTT.
042809              

072110 FD  NAPERSOFT-LETTERS
072110     LABEL RECORDS ARE STANDARD
072110     RECORDING MODE IS F
072110     BLOCK CONTAINS 0 RECORDS.
072110 01  NAPERSOFT-LETTER           PIC X(1500).

       WORKING-STORAGE SECTION.
       
NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
013017
013017 EXEC SQL
013017    INCLUDE SQLCA
013017 END-EXEC

NTTDel*77  SHORT                  PIC S9(4)  COMP-5 IS TYPEDEF.
NTTIns 77  SHORT                  PIC S9(4)  COMP-5 value zero.
013017 77  ws-sql-code                 pic s9(7) value zeros.
013017 77  ws-dis-sql-code             pic -9999999 value zeros.
013017 77  ws-connect-sw               pic x value spaces.
013017     88  connected-to-db           value 'Y'.
013017 77  ws-records-inserted         pic 9(5) value zeros.
013017
013017 01  P pointer.
013017 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
013017 01  var-ptr pointer.
013017 01  env-var-len                 pic 9(4)  binary.
013017 01  rc                          pic 9(9)  binary.
013017
013017 01  WS-KIXSYS.
013017     05  WS-KIX-FIL1             PIC X(10).
013017     05  WS-KIX-APPS             PIC X(10).
013017     05  WS-KIX-ENV              PIC X(10).
013017     05  WS-KIX-MYENV            PIC X(10).
013017     05  WS-KIX-SYS              PIC X(10).
013017
013017 EXEC SQL
013017    BEGIN DECLARE SECTION
013017 END-EXEC
013017
013017 01  sqlcmd                      pic x(1024).
013017 01  svr                         pic x(32).
013017 01  usr                         pic x(32).
013017 01  pass                        pic x(32).
013017 01  usr-pass                    pic x(64).
013017 01  ws-disp-code                pic s9(11).
013017 01  ws-rec-cntr                 pic s9(4) comp value +0.
013017 01  ws-test-date                pic x(10) value spaces.
013017 01  ws-moe-date                 PIC X(10).
013017 01  ws-ach-cashed-date          PIC X(10).
013017
013017 01  extract-record.
013017     05  ext-proc-dt             pic x(10).
013017     05  ext-bene                pic x(10).
013017     05  ext-carrier             pic x.
013017     05  ext-group               pic x(6).
013017     05  ext-state               pic xx.
013017     05  ext-account             pic x(10).
013017     05  ext-cert-eff-dt         pic x(10).
013017     05  ext-cert-no             pic x(11).
013017     05  ext-claim-no            pic x(7).
013017     05  ext-last-name           pic x(15).
013017     05  ext-first-name          pic x(12).
013017     05  ext-mid-init            pic x.
013017     05  ext-MEMBER-NAME         PIC X(30).
013017     05  ext-MEMBER-ADDRESS1     PIC X(30).
013017     05  ext-MEMBER-ADDRESS2     PIC X(30).
013017     05  ext-member-city         pic x(30).
013017     05  ext-member-state        pic xx.
013017     05  ext-MEMBER-ZIP-CODE     PIC X(9).
013017     05  ext-claim-type          pic x.
013017     05  ext-amt-paid            pic -zzzzz9.99.
013017     05  ext-amt-paid-a redefines
013017         ext-amt-paid            pic x(10).
013017     05  ext-paid-dt             pic x(10).
013017     05  ext-paid-from-dt        pic x(10).
013017     05  ext-paid-thru-dt        pic x(10).
013017     05  ext-check-no            pic x(10).
013017     05  ext-pay-type            pic x.
013017     05  ext-payee-name          pic x(30).
013017     05  ext-payee-addr1         pic x(30).
013017     05  ext-payee-addr2         pic x(30).
013017     05  ext-payee-CITY          pic x(30).
013017     05  ext-payee-STATE         pic xX.
013017     05  ext-payee-zip           pic x(10).
013017     05  ext-ach-aba-no          pic x(15).
013017     05  ext-ach-acct-no         pic x(20).
082219     05  ext-account-name        pic x(50).
013017
013017 EXEC SQL
013017    END DECLARE SECTION
013017 END-EXEC
       
       01  DRAFTS-WRITTEN                PIC S9(7) VALUE 0.
NTTDel*01  FILLER                          COMP-3.
NTTIns 01  FILLER.
           05  WS-CHECKS-WITHOUT-ADDRESSES PIC X       VALUE 'N'.
             88   CHECKS-WITHOUT-ADDRESSES             VALUE 'Y'.
041813     05  WS-SPECIAL-RELEASE-CHK      PIC X       VALUE ' '.
041813       88   SPECIAL-RELEASE-CHK                  VALUE 'X'.

       01  FILLER      COMP SYNC.
           05  WS-PAYMENT-NOTE-SEQ-NO      PIC S9(4)  COMP VALUE +0.
           05  WS-LAST-CONTROL-GROUP       PIC S9(8)   VALUE ZERO.
           05  WS-TIMES-PRINTED            PIC S9(4)   VALUE ZERO.
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.

       01  FILLER.
           05  WS-ACCOUNT-HOLD-RECORD      PIC X(2000).

           05  WS-LAST-CARRIER             PIC X     VALUE LOW-VALUES.

           05  WS-CHECK-WRITER-DATE        PIC XX VALUE LOW-VALUES.

           05  WS-AUTO-PAY-SW              PIC X       VALUE 'N'.

           05  WS-COV-TYPE                 PIC X.
           05  WS-KIND.
               10  WS-RETRO-DAYS           PIC 99.
               10  WS-RETRO-ELIM           PIC X.

           05  WS-PAYMENT-TYPE             PIC X       VALUE ZERO.
           05  WS-PAYEE-CODE.
               10  WS-PAYEE-CD             PIC X.
               10  WS-PAYEE-SEQ            PIC X.
               10  WS-PAYEE-SEQ-NUM REDEFINES
                   WS-PAYEE-SEQ            PIC 9.

           05  WS-SSN.
               10  WS-SSN-STATE            PIC XX.
               10  WS-SSN-ACCOUNT          PIC X(6).
               10  WS-SSN-LN3              PIC X(3).

           05  WS-MEMBER-NUMBER.
               10  WS-MEMBER-STATE         PIC XX.
               10  WS-MEMBER-ACCOUNT       PIC X(6).
               10  WS-MEMBER-LN4           PIC X(4).

           05  WS-ZIP-UNPACKED                 PIC 9(9)  VALUE ZEROS.

           05  WS-CARRIER-ADDRESS-DATA.
               10  WS-CARRIER-MAIL-TO-NAME     PIC X(30).
               10  WS-CARRIER-IN-CARE-OF       PIC X(30).
               10  WS-CARRIER-ADDRESS-LINE-1   PIC X(30).
               10  WS-CARRIER-ADDRESS-LINE-2   PIC X(30).
               10  WS-CARRIER-CITY-STATE       PIC X(30).
               10  WS-CARRIER-ZIP-CODE         PIC X(9).
               10  WS-CARRIER-PHONE-NO         PIC 9(11)     COMP-3.

           05  WS-WORK-PHONE               PIC X(10)   VALUE ZEROS.
           05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
                                           PIC 9(10).

           05  WS-JULIAN-YYDDD                 PIC 9(5).
030906
030906     05  WS-IND-PAST-PMT                 PIC X(1) VALUE 'N'.

091808     05  WS-PAYEE-STATE          PIC X(2).
091808     05  WS-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-BEG-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-END-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-STATE-LENGTH         PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-PAYEE-STATE-FOUND    PIC X(01)        VALUE 'N'.
091808         88 PAYEE-STATE-FOUND                     VALUE 'Y'.
091808     05  WS-PAYEE-CITY-STATE     PIC X(30).
091808     05  FILLER  REDEFINES WS-PAYEE-CITY-STATE.
091808         10  WS-PAYEE-CITY-ST OCCURS 30 TIMES PIC X(1).
042809
042809     05  WS-BIU-MSG.
042809         10  FILLER                  PIC X(11) 
042809             VALUE 'VIN NUMBER '.
042809         10  WS-BIU-VIN              PIC X(17) VALUE SPACES.
042809     05  WS-ELCRTT-KEY.
042809         10  WS-ELCRTT-PRIMARY       PIC X(33).
042809         10  WS-ELCRTT-REC-TYPE      PIC X(1).
042809     05  WS-BIU-BENEFIT-CODE         PIC X(2) VALUE SPACES.
042809         88  BIU-3-PMTS              VALUES '55','56'.
042809         88  BIU-X-PMTS              VALUES '??'.
042809     05  WS-ACCOUNT-NAME-LENGTH      PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-NAME-END-FOUND           PIC X(01)        VALUE 'N'.
091808         88 NAME-END-FOUND                            VALUE 'Y'.
042809     05  WS-ACCOUNT-NAME             PIC X(30).
042809     05  FILLER  REDEFINES  WS-ACCOUNT-NAME.
042809         10  WS-ACCT-NAME OCCURS 30 TIMES PIC X(1).
082218     05  WS-HOLD-UNTIL-DT            PIC X(02).

       01  PROGRAM-INTERFACE-BLOCK.
           12  PI-PROGRAM-WORK-AREA            PIC X(640).
           COPY ELC176PI.
                  16  FILLER PIC XX.

           COPY ELCCPA.

           COPY ELCNWA.

       01  EL352-INPUT.
           05  OPTION PIC  X(1).
           05  FILLER PIC  X(1).
           05  EL352-CONTROL-GROUPS        OCCURS 4 TIMES
                                           INDEXED BY EL352A-INDEX.
               10  EL352A-CONTROL-GROUP      PIC 9(7).
               10  EL352A-CONTROL-GROUP-X    REDEFINES
                   EL352A-CONTROL-GROUP      PIC X(7).
               10  FILLER PIC  X(1).

           COPY ELCDATE.

       01  MISC.
           05  MICR-STATUS          PIC XX            VALUE '00'.
           05  CHKQ-STATUS          PIC XX            VALUE '00'.
           05  CNTL-STATUS          PIC XX            VALUE '00'.
           05  TRLR-STATUS          PIC XX            VALUE '00'.
           05  CERT-STATUS          PIC XX            VALUE '00'.
           05  MSTR-STATUS          PIC XX            VALUE '00'.
           05  ACCT-STATUS          PIC XX            VALUE '00'.
           05  BENE-STATUS          PIC XX            VALUE '00'.
042809     05  ELCRTT-FILE-STATUS   PIC XX            VALUE '00'.
           05  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.
111714     05  ws-goback-ret-code  pic s9(4) comp value +0.
           05  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.
           05  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.
           05  WS-ZERO             PIC S9              VALUE ZERO.
           05  PGM-SUB             PIC S999    COMP-3  VALUE +020.
           05  SUB                 PIC S9(5) VALUE +0 COMP-3.
           05  SUB1                PIC S9(5) VALUE +0 COMP-3.
           05  SUB2                PIC S9(5) VALUE +0 COMP-3.

                                   COPY ELCDTECX.

                                   COPY ELCDTEVR.


       01  WS-FINAL-MESS-CONSTANT.
           05  WS-LINE-9                  PIC X(17)  VALUE
               'IF YOU HAVE ANY  '.
           05  WS-LINE-10                 PIC X(17)  VALUE
               'QUESTIONS, PLEASE'.
           05  WS-LINE-11                 PIC X(17)  VALUE
               'LET US KNOW.     '.

       01  S1                          PIC S999 VALUE +0 COMP-3.
       01  S2                          PIC S999 VALUE +0 COMP-3.

       01  FILLER.
           05  WS-WORK-FIELD1          PIC X(25).
           05  WS-WORK-FLD1 REDEFINES WS-WORK-FIELD1 OCCURS 25 PIC X.
           05  WS-LOAN-NUMBER          PIC X(10).
           05  WS-WORK-FLD2 REDEFINES WS-LOAN-NUMBER OCCURS 10 PIC X.

       01  WS-SOC-SEC-NO               PICTURE 9(18)   VALUE ZEROS.
       01  CSO-ZIP                     PIC Z(9).

       01  WS-DCC-REPLY-DT             PIC X(8)        VALUE SPACES.
       01  WS-DRAFT-ORDER              PIC 9(5)        VALUE ZEROS.
       01  WS-CHECK-AREA.
           05  COMPANY-CHECK           PICTURE X(2)    VALUE SPACE.
           05  FILLER                  PICTURE X(6)    VALUE SPACE.
       01  WS-COMPANY-NAME             PICTURE X(43)   VALUE SPACE.
       01  WS-COMPANY-NAME2            PICTURE X(43)   VALUE SPACE.
       01  CSO-COMPANY-HEADINGS.
           05  01-HEADING              PICTURE X(43)
LGC101         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
           05  02-HEADING              PICTURE X(43)
               VALUE '   CENTRAL STATES INDEMNITY CO. OF OMAHA   '.
           05  03-HEADING              PICTURE X(43)
               VALUE '        NATIONAL INDEMNITY COMPANY         '.
           05  04-HEADING              PICTURE X(43)
               VALUE ' MASSACHUSETTS INDEMNITY AND LIFE COMPANY  '.
           05  05-HEADING              PICTURE X(43)
               VALUE '        COLUMBIA INSURANCE COMPANY         '.
CSO685     05  06-HEADING              PICTURE X(43)
CSO685         VALUE '  NATIONAL FIRE AND MARINE INSURANCE CO.   '.
           05  07-HEADING              PIC X(43)
062006*        VALUE '   CSO FAMILY OF COMPANIES                 '.
080904         VALUE '   LENDERS PROTECTION ASSURANCE COMPANY    '.
       01  COMPANY-NAMES.
           05  01-COMP-NAME            PICTURE X(43)
LGC101         VALUE ' CENTRAL STATES HEALTH & LIFE CO. OF OMAHA '.
           05  02-COMP-NAME            PICTURE X(43)
               VALUE 'CENTRAL STATES INDEMNITY CO. OF OMAHA      '.
           05  03-COMP-NAME            PICTURE X(43)
               VALUE 'NATIONAL INDEMNITY COMPANY                 '.
           05  04-COMP-NAME            PICTURE X(43)
               VALUE 'MASSACHUSETTS INDEMNITY AND LIFE COMPANY   '.
           05  05-COMP-NAME            PICTURE X(43)
               VALUE 'COLUMBIA INSURANCE COMPANY                 '.
CSO685     05  06-COMP-NAME            PICTURE X(43)
CSO685         VALUE 'NATIONAL FIRE AND MARINE INSURANCE CO.     '.
           05  07-COMP-NAME            PIC X(43)
062006*        VALUE '   CSO FAMILY OF COMPANIES                 '.
080904         VALUE '   LENDERS PROTECTION ASSURANCE COMPANY    '.

       01  FILLER.

           05  WS-BIN-CURRENT-DT           PICTURE XX     VALUE SPACES.
           05  WS-EDT-CURRENT-DT           PICTURE X(8)   VALUE SPACES.

LGC007     05  WS-CPA-COMMENT.
LGC007         10  WS-CPA-COMMENT-P1       PIC X(29).
LGC007         10  WS-CPA-VOID-INDICATOR   PIC X.

      ***   MSG1 PARTIAL AH PAYMENT
       01  420C-MSG1.
           05  PIC X(35) VALUE 'A payment for benefits was made to '.
           05  PIC X(35) VALUE 'your account today.  Since more    '.
           05  PIC X(35) VALUE 'benefits may be payable in the futu'.
           05  PIC X(35) VALUE 're, we are enclosing another claim '.
           05  PIC X(35) VALUE 'form.  This form must be completed '.
           05  PIC X(35) VALUE 'IN FULL and sent to us when you    '.
032106     05  PIC X(35) VALUE 'return to work or on mm/dd/yy which'.
032106     05  PIC X(35) VALUE 'ever is sooner.  Thank you for your'.
           05  PIC X(35) VALUE 'cooperation.                       '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG1.
           05  420C-MSG1-LINE OCCURS 5 TIMES PIC X(70).
030906
      ***   MSG1 PARTIAL AH PAYMENT WHERE PD THRU DATE IS MORE
      ***      THAN ONE MONTH AND 1 DAY OLD
030906 01  420C-MSG1-PAST-PMT.
030906     05  PIC X(35) VALUE 'A payment for benefits was made to '.
030906     05  PIC X(35) VALUE 'your account today.  Since more    '.
030906     05  PIC X(35) VALUE 'benefits may be payable in the futu'.
030906     05  PIC X(35) VALUE 're, we are enclosing another claim '.
030906     05  PIC X(35) VALUE 'form.  This form must be completed '.
030906     05  PIC X(35) VALUE 'IN FULL and sent to us as soon as  '.
030906     05  PIC X(35) VALUE 'possible.  Thank you for your coope'.
030906     05  PIC X(35) VALUE 'ration.                            '.
030906     05  PIC X(35) VALUE '                                   '.
030906     05  PIC X(35) VALUE '                                   '.
030906 01  REDEFINES 420C-MSG1-PAST-PMT.
030906     05  420C-MSG1-PAST-PMT-LINE OCCURS 5 TIMES PIC X(70).

      ***   MSG2 FINAL AH PAYMENT
       01  420C-MSG2.
           05  PIC X(35) VALUE 'This is the last benefit payment fo'.
           05  PIC X(35) VALUE 'r this period of disability under  '.
           05  PIC X(35) VALUE 'this claim on your credit insurance'.
           05  PIC X(35) VALUE ' policy.  Please check with your   '.
           05  PIC X(35) VALUE 'financial institution within the ne'.
           05  PIC X(35) VALUE 'xt week to make sure it was        '.
           05  PIC X(35) VALUE 'credited to your account.          '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG2.
           05  420C-MSG2-LINE OCCURS 4 TIMES PIC X(70).

      ***  This msg3 does not appear to be used by this program
       01  420C-MSG3.
           05  PIC X(35) VALUE 'A claim payment was made on your ac'.
           05  PIC X(35) VALUE 'count today.  Please check with    '.
           05  PIC X(35) VALUE 'your financial institution within t'.
           05  PIC X(35) VALUE 'he next week to make sure it was   '.
           05  PIC X(35) VALUE 'credited to your account.          '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG3.
           05  420C-MSG3-LINE OCCURS 3 TIMES PIC X(70).

      ***   MSG4 FINAL LIFE PAYMENT
       01  420C-MSG4.
           05  PIC X(35) VALUE 'This payment represents the total b'.
           05  PIC X(35) VALUE 'enefits payable under this credit  '.
           05  PIC X(35) VALUE 'life insurance policy.  This claim '.
           05  PIC X(35) VALUE 'has been closed.                   '.
       01  REDEFINES 420C-MSG4.
           05  420C-MSG4-LINE OCCURS 2 TIMES PIC X(70).

      ***   MSG5 ADDITIONAL LIFE PAYMENT
       01  420C-MSG5.
           05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
           05  PIC X(35) VALUE 'e credit life insurance was        '.
           05  PIC X(35) VALUE 'purchased to protect this loan, we '.
           05  PIC X(35) VALUE 'have paid the loan off at the      '.
           05  PIC X(35) VALUE 'financial institution.  This paymen'.
           05  PIC X(35) VALUE 't to the estate represents the     '.
           05  PIC X(35) VALUE 'remaining benefit amount available '.
           05  PIC X(35) VALUE 'under this policy.  This claim has '.
           05  PIC X(35) VALUE 'been closed.                       '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG5.
CSODAN     05  420C-MSG5-LINE OCCURS 5 TIMES PIC X(70).

      ***  MSG6  CARRIER 8, FINAL AH PAYMENT
       01  420C-MSG6.
           05  PIC X(35) VALUE 'A claim payment associated with you'.
           05  PIC X(35) VALUE 'r Investors Heritage Life Insurance'.
           05  PIC X(35) VALUE 'Company coverage was issued today. '.
           05  PIC X(35) VALUE 'This draft copy represents this    '.
           05  PIC X(35) VALUE 'handling. We recommend you contact '.
           05  PIC X(35) VALUE 'your financial institution within  '.
           05  PIC X(35) VALUE 'the next week to ensure this paymen'.
           05  PIC X(35) VALUE 't was appropriately credited to    '.
           05  PIC X(35) VALUE 'your account.                      '.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG6.
           05  420C-MSG6-LINE OCCURS 5 TIMES PIC X(70).


      ***  MSG7  CARRIER 8, PARTIAL AH PAYMENT
       01  420C-MSG7.
           05  PIC X(35) VALUE 'A claim payment associated with you'.
           05  PIC X(35) VALUE 'r Investors Heritage Life Insurance'.
           05  PIC X(35) VALUE 'Company coverage was issued today. '.
           05  PIC X(35) VALUE 'Since more benefits may be payable '.
           05  PIC X(35) VALUE 'in the future, we are enclosing ano'.
           05  PIC X(35) VALUE 'ther claim form. This form must be '.
           05  PIC X(35) VALUE 'completed IN FULL and sent to us wh'.
           05  PIC X(35) VALUE 'en you return to work or on        '.
032106     05  PIC X(35) VALUE 'mm/dd/yy whichever is sooner. Thank'.
032106     05  PIC X(35) VALUE ' you for your cooperation.         '.
       01  REDEFINES 420C-MSG7.
           05  420C-MSG7-LINE OCCURS 5 TIMES PIC X(70).

      ***  MSG7  CARRIER 8, PARTIAL AH PAYMENT WHERE PD THRU DATE IS
      ***      MORE THAN ONE MONTH AND 1 DAY OLD
030906 01  420C-MSG7-PAST-PMT.
           05  PIC X(35) VALUE 'A claim payment associated with you'.
           05  PIC X(35) VALUE 'r Investors Heritage Life Insurance'.
           05  PIC X(35) VALUE 'Company coverage was issued today. '.
           05  PIC X(35) VALUE 'Since more benefits may be payable '.
           05  PIC X(35) VALUE 'in the future, we are enclosing ano'.
           05  PIC X(35) VALUE 'ther claim form. This form must be '.
           05  PIC X(35) VALUE 'completed IN FULL and sent to us as'.
           05  PIC X(35) VALUE ' soon as possible. Thank you for   '.
032106     05  PIC X(35) VALUE 'your cooperation.                  '.
032106     05  PIC X(35) VALUE '                                   '.
030906 01  REDEFINES 420C-MSG7-PAST-PMT.
030906     05  420C-MSG7-PAST-PMT-LINE OCCURS 5 TIMES PIC X(70).

      ***   MSG8 CARRIER 8, ADDITIONAL LIFE PAYMENT
       01  420C-MSG8.
           05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
           05  PIC X(35) VALUE 'e credit life insurance was        '.
           05  PIC X(35) VALUE 'purchased to protect this loan, our'.
           05  PIC X(35) VALUE ' payment issued to the financial   '.
           05  PIC X(35) VALUE 'institution should have paid the ou'.
           05  PIC X(35) VALUE 'tstanding balance in full. This    '.
           05  PIC X(35) VALUE 'payment issued to the Estate, repre'.
           05  PIC X(35) VALUE 'sents remaining benefits associated'.
           05  PIC X(35) VALUE 'with the decedent''s Investors Herit'.
           05  PIC X(35) VALUE 'age Life Insurance Company         '.
           05  PIC X(35) VALUE 'coverage. This claim is now closed.'.
           05  PIC X(35) VALUE '                                   '.
       01  REDEFINES 420C-MSG8.
CSODAN     05  420C-MSG8-LINE OCCURS 6 TIMES PIC X(70).

      ***   MSG9 CARRIER 8, FINAL LIFE PAYMENT
       01  420C-MSG9.
           05  PIC X(35) VALUE 'This payment represents total benef'.
           05  PIC X(35) VALUE 'its payable under the Investors    '.
           05  PIC X(35) VALUE 'Heritage Life Insurance coverage. T'.
           05  PIC X(35) VALUE 'his claim has been closed.         '.
       01  REDEFINES 420C-MSG9.
           05  420C-MSG9-LINE OCCURS 2 TIMES PIC X(70).

      ***   CLAIM TYPE 'I', PARTIAL PAYMENTS PMT 1
081905 01  DCC1-MSG1.
081905     05  PIC X(35) VALUE 'Payment was issued under your optio'.
081905     05  PIC X(35) VALUE 'nal debt cancellation contract. As '.
081905     05  PIC X(35) VALUE 'additional benefits may be due, a "'.
032106     05  PIC X(35) VALUE 'Proof of Continuing Involuntary    '.
081905     05  PIC X(35) VALUE 'Unemployment" form is enclosed and '.
081905     05  PIC X(35) VALUE 'must be completed IN FULL.         '.
081905     05  PIC X(35) VALUE 'Return it to us when you return to '.
081905     05  PIC X(35) VALUE 'work or on mm/dd/yy, whichever is  '.
081905     05  PIC X(35) VALUE 'sooner. If you are receiving benefi'.
081905     05  PIC X(35) VALUE 'ts from a state job service,       '.
081905     05  PIC X(35) VALUE 'supporting documentation is needed '.
081905     05  PIC X(35) VALUE 'as outlined on the enclosed form.  '.
081905 01  REDEFINES DCC1-MSG1.
081905     05  DCC1-MSG1-LINE OCCURS 6 TIMES PIC X(70).
030906
030906***   CLAIM TYPE 'I', PARTIAL PAYMENTS PMT 1 PAST DATE
030906 01  DCC1-MSG1-PAST-PMT.
030906     05  PIC X(35) VALUE 'Payment was issued under your optio'.
030906     05  PIC X(35) VALUE 'nal debt cancellation contract. As '.
030906     05  PIC X(35) VALUE 'additional benefits may be due, a "'.
030906     05  PIC X(35) VALUE 'Proof of Continuing Involuntary    '.
030906     05  PIC X(35) VALUE 'Unemployment" form is enclosed and '.
030906     05  PIC X(35) VALUE 'must be completed IN FULL.         '.
030906     05  PIC X(35) VALUE 'Return it to us as soon as possible'.
030906     05  PIC X(35) VALUE '. If you are receiving benefits    '.
030906     05  PIC X(35) VALUE 'from a state job service, supportin'.
030906     05  PIC X(35) VALUE 'g documentation is needed as       '.
030906     05  PIC X(35) VALUE 'outlined on the enclosed form. Than'.
030906     05  PIC X(35) VALUE 'k you for your cooperation.        '.
030906 01  REDEFINES DCC1-MSG1-PAST-PMT.
030906     05  DCC1-MSG1-PAST-PMT-LINE OCCURS 6 TIMES PIC X(70).

      ***   CLAIM TYPE 'I', FINAL PAYMENTS, PMT 2
011105 01  DCC1-MSG2.
011105     05  PIC X(35) VALUE 'This is our final payment under the'.
011105     05  PIC X(35) VALUE ' Involuntary Unemployment portion  '.
011105     05  PIC X(35) VALUE 'of your debt cancellation contract.'.
011105     05  PIC X(35) VALUE ' We recommend you contact the      '.
011105     05  PIC X(35) VALUE 'financial institution to ensure all'.
011105     05  PIC X(35) VALUE ' payments have been appropriately  '.
011105     05  PIC X(35) VALUE 'credited to your loan balance. This'.
081905     05  PIC X(35) VALUE ' file is now closed, as our        '.
081905     05  PIC X(35) VALUE 'handling is complete.              '.
081905     05  PIC X(35) VALUE '                                   '.
011105 01  REDEFINES DCC1-MSG2.
011105     05  DCC1-MSG2-LINE OCCURS 5 TIMES PIC X(70).
042809
042809***   CLAIM TYPE 'I', BIU 3 PAYMENTS  INITIAL PMT
042809****     FIRST TWO LINES ARE RESERVED FOR VIN
042809 01  DCC1-BIU-MSG1.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'Your claim for Involuntary Unemploy'.
042809     05  PIC X(35) VALUE 'ment Benefits has been approved.   '.
042809     05  PIC X(35) VALUE 'Under your contract, you qualify fo'.
042809     05  PIC X(35) VALUE 'r three monthly benefit payments   '.
042809     05  PIC X(35) VALUE 'under the Complimentary Involuntary'.
042809     05  PIC X(35) VALUE ' Unemployment program offered by   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809 01  REDEFINES DCC1-BIU-MSG1.
042809     05  DCC1-BIU-MSG1-LINE OCCURS 6 TIMES PIC X(70).
042809
042809 01  DCC1-BIU-MSG1-ACCT-LINE.
042809     05  DCC1-BIU1-ACCT37     PIC X(37)
042809         VALUE 'Two more payments will be made under '.
042809     05  DCC1-BIU1-ACCT42     PIC X(5)  VALUE 'your '.
042809     05  DCC1-BIU1-ACCT52     PIC X(10) VALUE 'contract, '.
042809     05  DCC1-BIU1-ACCT-END   PIC X(20) 
042809         VALUE 'in 30 day intervals.'. 
042809
042809***   CLAIM TYPE 'I', BIU  3 PAYMENTS SECOND PMT
042809****     FIRST TWO LINES ARE RESERVED FOR VIN
042809 01  DCC1-BIU-MSG2.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'Your second benefit has been issued'.
042809     05  PIC X(35) VALUE ' under the Complimentary           '.
042809     05  PIC X(35) VALUE 'Involuntary Unemployment program of'.
042809     05  PIC X(35) VALUE 'fered by                           '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809 01  REDEFINES DCC1-BIU-MSG2.
042809     05  DCC1-BIU-MSG2-LINE OCCURS 6 TIMES PIC X(70).
042809
042809 01  DCC1-BIU-MSG2-ACCT-LINE.
042809     05  DCC1-BIU2-ACCT38     PIC X(38)
042809         VALUE 'Your final payment will be made under '.
042809     05  DCC1-BIU2-ACCT-END   PIC X(25) 
042809         VALUE 'your contract in 30 days.'. 
042809
042809***   CLAIM TYPE 'I', BIU  X PAYMENTS PARTIAL PMT
042809****     FIRST TWO LINES ARE RESERVED FOR VIN
042809 01  DCC1-BIU-MSG3.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'A benefit has been issued under the'.
042809     05  PIC X(35) VALUE ' Complimentary Involuntary         '.
042809     05  PIC X(35) VALUE 'Unemployment program offered by    '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'As additional benefits may be due, '.
042809     05  PIC X(35) VALUE 'a "Proof of Continuing Involuntary '.
042809     05  PIC X(35) VALUE 'Unemployment" form is enclosed and '.
042809     05  PIC X(35) VALUE 'must be completed IN FULL.  Return '.
042809     05  PIC X(35) VALUE 'it to us when you return to work or'.
042809     05  PIC X(35) VALUE ' on MM/DD/YY, whichever is sooner. '.
042809 01  REDEFINES DCC1-BIU-MSG3.
042809     05  DCC1-BIU-MSG3-LINE OCCURS 6 TIMES PIC X(70).
042809
042809***   CLAIM TYPE 'I', BIU PAYMENTS  FINAL PMT ALL BIU PLANS 
042809****     FIRST TWO LINES ARE RESERVED FOR VIN
042809 01  DCC1-BIU-MSG4.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'A final benefit has been issued und'.
042809     05  PIC X(35) VALUE 'er the Complimentary Involuntary   '.
042809     05  PIC X(35) VALUE 'Unemployment program offered by    '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE 'Benefits have been paid in full und'.
042809     05  PIC X(35) VALUE 'er the terms of your contract.     '.
042809     05  PIC X(35) VALUE 'No future benefits are payable unde'.
042809     05  PIC X(35) VALUE 'r this contract.                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809     05  PIC X(35) VALUE '                                   '.
042809 01  REDEFINES DCC1-BIU-MSG4.
042809     05  DCC1-BIU-MSG4-LINE OCCURS 6 TIMES PIC X(70).
042809
      ***   CLAIM TYPE 'G', FINAL AND SETTLE PAYMENTS, PMT 2 & 3.
011105 01  DCC1-MSG3.
011105     05  PIC X(35) VALUE 'This payment represents benefits du'.
011105     05  PIC X(35) VALUE 'e under the Loan Deficiency Waiver '.
011105     05  PIC X(35) VALUE 'portion of your debt cancellation c'.
011105     05  PIC X(35) VALUE 'ontract.  We recommend you contact '.
011105     05  PIC X(35) VALUE 'the financial institution to ensure'.
011105     05  PIC X(35) VALUE ' this payment was appropriately    '.
081905     05  PIC X(35) VALUE 'credited to your account.  This fil'.
081905     05  PIC X(35) VALUE 'e is now closed, as our handling is'.
081905     05  PIC X(35) VALUE 'complete.                          '.
081905     05  PIC X(35) VALUE '                                   '.
011105 01  REDEFINES DCC1-MSG3.
011105     05  DCC1-MSG3-LINE OCCURS 5 TIMES PIC X(70).

      ***   CLAIM TYPE 'L', FINAL AND ADDL PAYMENTS, PMT 2 OR 4.
081905 01  DCC1-MSG4.
081905     05  PIC X(35) VALUE 'We are sorry for your loss and wish'.
081905     05  PIC X(35) VALUE ' to extend our sincere condolences.'.
081905     05  PIC X(35) VALUE 'This payment represents benefits du'.
081905     05  PIC X(35) VALUE 'e under the Loss of Life/Accidental'.
081905     05  PIC X(35) VALUE 'Death portion of the debt cancellat'.
081905     05  PIC X(35) VALUE 'ion contract.  We recommend you    '.
081905     05  PIC X(35) VALUE 'contact the financial institution t'.
081905     05  PIC X(35) VALUE 'o ensure this payment was          '.
081905     05  PIC X(35) VALUE 'appropriately credited to the loan '.
081905     05  PIC X(35) VALUE 'account. This file is now closed,  '.
081905     05  PIC X(35) VALUE 'as our handling is complete.       '.
081905     05  PIC X(35) VALUE '                                   '.
081905 01  REDEFINES DCC1-MSG4.
081905     05  DCC1-MSG4-LINE OCCURS 6 TIMES PIC X(70).

      ***   CLAIM TYPE 'A', FINAL PAYMENTS PMT 2
081905 01  DCC1-MSG5.
081905     05  PIC X(35) VALUE 'This is our final payment under the'.
081905     05  PIC X(35) VALUE ' Total Disability Protection       '.
081905     05  PIC X(35) VALUE 'portion of your debt cancellation c'.
081905     05  PIC X(35) VALUE 'ontract. We recommend you contact  '.
081905     05  PIC X(35) VALUE 'the financial institution to ensure'.
081905     05  PIC X(35) VALUE ' all payments have been            '.
081905     05  PIC X(35) VALUE 'appropriately credited to your loan'.
081905     05  PIC X(35) VALUE ' balance. This file is now closed, '.
081905     05  PIC X(35) VALUE 'as our handling is complete.       '.
081905     05  PIC X(35) VALUE '                                   '.
081905 01  REDEFINES DCC1-MSG5.
081905     05  DCC1-MSG5-LINE OCCURS 5 TIMES PIC X(70).

      ***   CLAIM TYPE 'A', PARTIAL PAYMENTS PMT 1
081905 01  DCC1-MSG6.
081905     05  PIC X(35) VALUE 'This payment is being issued under '.
081905     05  PIC X(35) VALUE 'your optional debt cancellation    '.
081905     05  PIC X(35) VALUE 'contract.  Because additional benef'.
081905     05  PIC X(35) VALUE 'its for total disability may be    '.
081905     05  PIC X(35) VALUE 'due, a "Proof of Continuing Total D'.
081905     05  PIC X(35) VALUE 'isability" form is enclosed.  This '.
081905     05  PIC X(35) VALUE 'form must be completed IN FULL and '.
081905     05  PIC X(35) VALUE 'should be returned to us when you  '.
081905     05  PIC X(35) VALUE 'return to work or on mm/dd/yy, whic'.
081905     05  PIC X(35) VALUE 'hever is sooner.  Thank you for    '.
081905     05  PIC X(35) VALUE 'your cooperation.                  '.
081905     05  PIC X(35) VALUE '                                   '.
081905 01  REDEFINES DCC1-MSG6.
081905     05  DCC1-MSG6-LINE OCCURS 6 TIMES PIC X(70).
030906
030906***   CLAIM TYPE 'A', PARTIAL PAYMENTS PMT 1 PAST DATE
030906 01  DCC1-MSG6-PAST-PMT.
030906     05  PIC X(35) VALUE 'This payment is being issued under '.
030906     05  PIC X(35) VALUE 'your optional debt cancellation    '.
030906     05  PIC X(35) VALUE 'contract.  Because additional benef'.
030906     05  PIC X(35) VALUE 'its for total disability may be    '.
030906     05  PIC X(35) VALUE 'due, a "Proof of Continuing Total D'.
030906     05  PIC X(35) VALUE 'isability" form is enclosed.  This '.
030906     05  PIC X(35) VALUE 'form must be completed IN FULL and '.
030906     05  PIC X(35) VALUE 'should be returned to us as soon as'.
030906     05  PIC X(35) VALUE 'possible.  Thank you for your coope'.
030906     05  PIC X(35) VALUE 'ration.                            '.
030906     05  PIC X(35) VALUE '                                   '.
030906     05  PIC X(35) VALUE '                                   '.
030906 01  REDEFINES DCC1-MSG6-PAST-PMT.
030906     05  DCC1-MSG6-PAST-PMT-LINE OCCURS 6 TIMES PIC X(70).

011105*01  DCC1-MSG5.
011105*    05  PIC X(35) VALUE 'We are sorry for your loss.  Becaus'.
011105*    05  PIC X(35) VALUE 'e debt protection coverage was     '.
011105*    05  PIC X(35) VALUE 'purchased to protect this loan, we '.
011105*    05  PIC X(35) VALUE 'have made a payment to the         '.
011105*    05  PIC X(35) VALUE 'financial institution.  The file ha'.
011105*    05  PIC X(35) VALUE 'been closed.                       '.
011105*01  REDEFINES DCC1-MSG5.
011105*    05  DCC1-MSG5-LINE OCCURS 3 TIMES PIC X(70).

                                  COPY ELC176W3.

                                  COPY ELCCSOCL.
072110
072110 01  WORK-AREA.
072110
072110     05  WRK-LOAN-NUMBER      PIC X(25).
072110     05  WRK-LOAN-NUMBER-R REDEFINES WRK-LOAN-NUMBER.
072110         10  WRK-LOAN-NO-CHAR OCCURS 25 TIMES PIC X.
072110
072110     05  WRK-END-FOUND           PIC X   VALUE 'N'.
072110         88 END-FOUND                    VALUE 'Y'.
072110
072110     05  WRK-CHECK-DATE       PIC X(10).
072110     05  WRK-PAID-FROM-DT     PIC X(10).
072110     05  WRK-PAID-THRU-DT     PIC X(10).
072110     05  WRK-FORM-DUE-DT      PIC X(10).
072110     05  WRK-PREV-PAID-THRU   PIC X(10).
072110     05  WRK-PREV-TOT-AMT     PIC S9(7)V99 COMP-3 VALUE +0.
072110     05  WRK-PREV-PMTS        PIC S9(3)    COMP-3 VALUE +0.
072110     05  WRK-MO-BENEFIT       PIC S9(7)V99 COMP-3 VALUE +0.
072110     05  WRK-HOLD-EXP-DT      PIC XX.
072110     05  WRK-CERT-EXP-DT      PIC X(10).
072110     05  WRK-AUTO-PAY-END-DT  PIC X(10).
030311     05  WRK-COMPANY          PIC 99      COMP VALUE ZERO.    
032311     05  WRK-HOLD-BENEFIT-CD  PIC X(2).
072110
072110     05  WRK-PAYEE-TOTALS.
072110         10  WRK-PAYEE-NAME-1     PIC X(30).
072110         10  WRK-TOTAL-AMT-PAID-1 PIC S9(7)V99 COMP-3.
072110         10  WRK-PAID-FROM-1      PIC X(10).
072110         10  WRK-PAID-THRU-1      PIC X(10).
072110         10  WRK-SAVE-FROM-1      PIC X(2).
072110         10  WRK-SAVE-THRU-1      PIC X(2).
072110         10  WRK-PAYEE-NAME-2     PIC X(30).
072110         10  WRK-TOTAL-AMT-PAID-2 PIC S9(7)V99 COMP-3.
072110         10  WRK-PAID-FROM-2      PIC X(10).
072110         10  WRK-PAID-THRU-2      PIC X(10).
072110         10  WRK-SAVE-FROM-2      PIC X(2).
072110         10  WRK-SAVE-THRU-2      PIC X(2).
072110         10  WRK-PAYEE-NAME-3     PIC X(30).
072110         10  WRK-TOTAL-AMT-PAID-3 PIC S9(7)V99 COMP-3.
072110         10  WRK-PAID-FROM-3      PIC X(10).
072110         10  WRK-PAID-THRU-3      PIC X(10).
072110         10  WRK-SAVE-FROM-3      PIC X(2).
072110         10  WRK-SAVE-THRU-3      PIC X(2).
072110         10  WRK-PAYEE-NAME-4     PIC X(30).
072110         10  WRK-TOTAL-AMT-PAID-4 PIC S9(7)V99 COMP-3.
072110         10  WRK-PAID-FROM-4      PIC X(10).
072110         10  WRK-PAID-THRU-4      PIC X(10).
072110         10  WRK-SAVE-FROM-4      PIC X(2).
072110         10  WRK-SAVE-THRU-4      PIC X(2).
072110
072110     05  WRK-PRINT-EOB-IND    PIC X.
020413     05  WRK-PRINT-CLM-FORM   PIC X.
020413     05  WRK-PRINT-SURVEY     PIC X.
072110     05  WRK-NOTE-CODE-IND    PIC X.
072110     05  WRK-DRAFT-NOTE-2.
072110         10  WRK-NOTE-CODE-1  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-2  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-3  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-4  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-5  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-6  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-7  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-8  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-9  PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-10 PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-11 PIC X(4).
072110         10  FILLER           PIC X(1).
072110         10  WRK-NOTE-CODE-12 PIC X(4).
072110         10  FILLER           PIC X(1).
072110
032311 01  WRK-EOB-NOTE-CODES.
032311     05  WRK-EOB-SUB         PIC S9(3) COMP-3 VALUE +1.
032311     05  WRK-EOB-CODES       PIC X(48)    VALUE SPACES.
032311     05  FILLER REDEFINES WRK-EOB-CODES.
032311         10 WRK-EOB-LU-CODE OCCURS 12 TIMES PIC X(4).
032311
072110 01  CID-BARCODE.
030311     05  BC-COMPANY          PIC 9(1)     VALUE ZERO.
030311     05  BC-CARRIER          PIC X        VALUE SPACE.
072110     05  BC-CLAIM-NO         PIC X(7)     VALUE SPACE.
030311     05  BC-CERT-NO          PIC X(11)    VALUE SPACE.
030311     05  BC-SEQ              PIC 9(5)     VALUE ZERO.
072110
072110 01  NAP-LETTER-RECORD.
072110     05  NLR-CYCLE-DATE       PIC X(8).
072110     05  NLR-TAB00            PIC X.
072110     05  NLR-LETTER-ID        PIC X(4).
072110     05  NLR-TAB01            PIC X.
072110     05  NLR-CARRIER          PIC X.
072110     05  NLR-TAB02            PIC X.
072110     05  NLR-STATE-CODE       PIC X(2).
072110     05  NLR-TAB03            PIC X.
072110     05  NLR-CLAIM-NO         PIC X(7).
072110     05  NLR-TAB04            PIC X.
072110     05  NLR-CERT-NO          PIC X(11).
072110     05  NLR-TAB05            PIC X.
072110     05  NLR-ACCT-NO          PIC X(10).
072110     05  NLR-TAB06            PIC X.
072110     05  NLR-CERT-EFF-DT      PIC X(10).
072110     05  NLR-TAB07            PIC X.
072110     05  NLR-CLAIM-INCUR-DT   PIC X(10).
072110     05  NLR-TAB08            PIC X.
072110     05  NLR-DRAFT-DT         PIC X(10).
072110     05  NLR-TAB09            PIC X.
072110     05  NLR-DRAFT-NO         PIC X(10).
072110     05  NLR-TAB10            PIC X.
072110     05  NLR-DRAFT-ORDER      PIC X(5).
072110     05  NLR-TAB11            PIC X.
072110     05  NLR-DRAFT-AMOUNT     PIC -9(7).99.
072110     05  NLR-TAB12            PIC X.
072110     05  NLR-PAID-FROM-DT     PIC X(10).
072110     05  NLR-TAB13            PIC X.
072110     05  NLR-PAID-THRU-DT     PIC X(10).
072110     05  NLR-TAB14            PIC X.
072110     05  NLR-FORM-DUE-DT      PIC X(10).
072110     05  NLR-TAB15            PIC X.
072110     05  NLR-PAYEE-NAME       PIC X(30).
072110     05  NLR-TAB16            PIC X.
072110     05  NLR-PAYEE-ADDR1      PIC X(40).
072110     05  NLR-TAB17            PIC X.
072110     05  NLR-PAYEE-ADDR2      PIC X(40).
072110     05  NLR-TAB18            PIC X.
072110     05  NLR-PAYEE-CITY       PIC X(40).
072110     05  NLR-TAB19            PIC X.
072110     05  NLR-PAYEE-STATE      PIC X(2).
072110     05  NLR-TAB20            PIC X.
072110     05  NLR-PAYEE-ZIP        PIC X(9).
072110     05  NLR-TAB21            PIC X.
072110     05  NLR-INS-LAST-NAME    PIC X(15).
072110     05  NLR-TAB22            PIC X.
072110     05  NLR-INS-FIRST-NAME   PIC X(15).
072110     05  NLR-TAB23            PIC X.
072110     05  NLR-INS-ADDR1        PIC X(40).
072110     05  NLR-TAB24            PIC X.
072110     05  NLR-INS-ADDR2        PIC X(40).
072110     05  NLR-TAB25            PIC X.
072110     05  NLR-INS-CITY         PIC X(40).
072110     05  NLR-TAB26            PIC X.
072110     05  NLR-INS-STATE        PIC X(2).
072110     05  NLR-TAB27            PIC X.
072110     05  NLR-INS-ZIP          PIC X(9).
072110     05  NLR-TAB28            PIC X.
072110     05  NLR-ACCT-NAME        PIC X(30).
072110     05  NLR-TAB29            PIC X.
072110     05  NLR-ACCT-ADDR1       PIC X(40).
072110     05  NLR-TAB30            PIC X.
072110     05  NLR-ACCT-ADDR2       PIC X(40).
072110     05  NLR-TAB31            PIC X.
072110     05  NLR-ACCT-CITY        PIC X(40).
072110     05  NLR-TAB32            PIC X.
072110     05  NLR-ACCT-STATE       PIC X(2).
072110     05  NLR-TAB33            PIC X.
072110     05  NLR-ACCT-ZIP         PIC X(9).
072110     05  NLR-TAB34            PIC X.
072110     05  NLR-BENE-NAME        PIC X(30).
072110     05  NLR-TAB35            PIC X.
072110     05  NLR-BENE-ADDR1       PIC X(40).
072110     05  NLR-TAB36            PIC X.
072110     05  NLR-BENE-ADDR2       PIC X(40).
072110     05  NLR-TAB37            PIC X.
072110     05  NLR-BENE-CITY        PIC X(40).
072110     05  NLR-TAB38            PIC X.
072110     05  NLR-BENE-STATE       PIC X(2).
072110     05  NLR-TAB39            PIC X.
072110     05  NLR-BENE-ZIP         PIC X(9).
072110     05  NLR-TAB40            PIC X.
072110     05  NLR-NO-OF-PMTS       PIC ZZ9.
072110     05  NLR-TAB41            PIC X.
072110     05  NLR-TOTAL-PAID-AMT   PIC -9(7).99.
072110     05  NLR-TAB42            PIC X.
072110     05  NLR-PREV-PAID-THRU-DT PIC X(10).
072110     05  NLR-TAB43            PIC X.
072110     05  NLR-PREV-TOT-PAID-AMT PIC -9(7).99.
072110     05  NLR-TAB44            PIC X.
072110     05  NLR-PREV-NO-OF-PMTS  PIC ZZ9.
072110     05  NLR-TAB45            PIC X.
072110     05  NLR-MONTHLY-BENEFIT  PIC -9(7).99.
072110     05  NLR-TAB46            PIC X.
072110     05  NLR-LOAN-NO          PIC X(25).
072110     05  NLR-TAB47            PIC X.
072110     05  NLR-EOB-LOAN-NO      PIC X(25).
072110     05  NLR-TAB48            PIC X.
072110     05  NLR-COV-LET-ID       PIC X(4).
072110     05  NLR-TAB49            PIC X.
072110     05  NLR-SURVEY-ID        PIC X(4).
072110     05  NLR-TAB50            PIC X.
072110     05  NLR-DCC-EVENT        PIC X(12).
072110     05  NLR-TAB51            PIC X.
072110     05  NLR-EOB-ID           PIC X(4).
072110     05  NLR-TAB52            PIC X.
072110     05  NLR-ENCLOSURE-CD     PIC X(3).
072110     05  NLR-TAB53            PIC X.
072110     05  NLR-DRAFT-NOTE-1     PIC X(60).
072110     05  NLR-TAB54            PIC X.
072110     05  NLR-DRAFT-NOTE-2     PIC X(60).
072110     05  NLR-TAB55            PIC X.
072110     05  NLR-DRAFT-NOTE-3     PIC X(4).
072110     05  NLR-TAB56            PIC X.
072110     05  NLR-DRAFT-NOTE-4     PIC X(4).
072110     05  NLR-TAB57            PIC X.
072110     05  NLR-DRAFT-NOTE-5     PIC X(4).
072110     05  NLR-TAB58            PIC X.
072110     05  NLR-DRAFT-NOTE-6     PIC X(4).
072110     05  NLR-TAB59            PIC X.
072110     05  NLR-DRAFT-NOTE-7     PIC X(4).
072110     05  NLR-TAB60            PIC X.
072110     05  NLR-DRAFT-NOTE-8     PIC X(4).
072110     05  NLR-TAB61            PIC X.
072110     05  NLR-DRAFT-NOTE-9     PIC X(4).
072110     05  NLR-TAB62            PIC X.
072110     05  NLR-DRAFT-NOTE-10    PIC X(4).
072110     05  NLR-TAB63            PIC X.
072110     05  NLR-DRAFT-NOTE-11    PIC X(4).
072110     05  NLR-TAB64            PIC X.
072110     05  NLR-DRAFT-NOTE-12    PIC X(4).
072110     05  NLR-TAB65            PIC X.
072110     05  NLR-DRAFT-NOTE-13    PIC X(4).
072110     05  NLR-TAB66            PIC X.
072110     05  NLR-DRAFT-NOTE-14    PIC X(4).
072110     05  NLR-TAB67            PIC X.
072110     05  NLR-CLAIM-TYPE       PIC X.
072110     05  NLR-TAB68            PIC X.
072110     05  NLR-PMT-TYPE         PIC X.
072110     05  NLR-TAB69            PIC X.
072110     05  NLR-PLAN-CODE        PIC X(4).
072110     05  NLR-TAB70            PIC X.
072110     05  NLR-BARCODE          PIC X(25).
072110     05  NLR-TAB71            PIC X.
072110     05  NLR-LIFE-INT-RATE    PIC 99.9(5).
072110     05  NLR-TAB72            PIC X.
072110     05  NLR-INS-SEX-CODE     PIC X.
072110     05  NLR-TAB73            PIC X.
072110     05  NLR-AUTO-PAY         PIC X.
072110     05  NLR-TAB74            PIC X.
072110     05  NLR-AUTO-PAY-END-DT  PIC X(10).
072110     05  NLR-TAB75            PIC X.
072110     05  NLR-CERT-EXP-DT      PIC X(10).
072110     05  NLR-TAB76            PIC X.
072110     05  NLR-PAYEE-NAME-1     PIC X(30).
072110     05  NLR-TAB77            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-1 PIC -9(7).99.
072110     05  NLR-TAB78            PIC X.
072110     05  NLR-PAID-FROM-1      PIC X(10).
072110     05  NLR-TAB79            PIC X.
072110     05  NLR-PAID-THRU-1      PIC X(10).
072110     05  NLR-TAB80            PIC X.
072110     05  NLR-PAYEE-NAME-2     PIC X(30).
072110     05  NLR-TAB81            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-2 PIC -9(7).99.
072110     05  NLR-TAB82            PIC X.
072110     05  NLR-PAID-FROM-2      PIC X(10).
072110     05  NLR-TAB83            PIC X.
072110     05  NLR-PAID-THRU-2      PIC X(10).
072110     05  NLR-TAB84            PIC X.
072110     05  NLR-PAYEE-NAME-3     PIC X(30).
072110     05  NLR-TAB85            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-3 PIC -9(7).99.
072110     05  NLR-TAB86            PIC X.
072110     05  NLR-PAID-FROM-3      PIC X(10).
072110     05  NLR-TAB87            PIC X.
072110     05  NLR-PAID-THRU-3      PIC X(10).
072110     05  NLR-TAB88            PIC X.
072110     05  NLR-PAYEE-NAME-4     PIC X(30).
072110     05  NLR-TAB89            PIC X.
072110     05  NLR-TOTAL-AMT-PAID-4 PIC -9(7).99.
072110     05  NLR-TAB90            PIC X.
072110     05  NLR-PAID-FROM-4      PIC X(10).
072110     05  NLR-TAB91            PIC X.
072110     05  NLR-PAID-THRU-4      PIC X(10).
072110     05  NLR-TAB92            PIC X.
092010     05  NLR-NEXTBUSDATE      PIC X(10).
092010     05  NLR-TAB93            PIC X.
092010     05  NLR-PAYEE-CODE       PIC XX.
092010     05  NLR-TAB94            PIC X.
031612     05  NLR-AHL-CLAIM-NO     PIC X(9).
031612     05  NLR-TAB95            PIC X.
072110     05  NLR-LAST-BYTE        PIC X.
072110                            
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

072110 LINKAGE SECTION.
072110
072110 01  PARM.
072110     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
072110     05  PARM-CYCLE-DATE             PIC X(08)     VALUE SPACES.
           05  parm-2-bank-days            pic x(08)     value spaces.
       01  var                         pic x(30).
072110
072110 PROCEDURE DIVISION USING PARM.
072110
072110      IF PARM-CYCLE-DATE = SPACES
072110          DISPLAY 'MISSING CYCLE DATE PARM'
072110          GO TO ABEND-PGM
072110      END-IF.

                                   COPY ELCDTERX.

013017     set P to address of KIXSYS
013017     CALL "getenv" using by value P returning var-ptr
013017     if var-ptr = null then
013017        display ' kixsys not set '
013017     else
013017        set address of var to var-ptr
013017        move 0 to env-var-len
013017        inspect var tallying env-var-len
013017          for characters before X'00' 
013017        unstring var (1:env-var-len) delimited by '/'
013017           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
013017              WS-KIX-SYS
013017        end-unstring
013017     end-if
013017
013017     display ' KIXSYS = ' ws-kix-myenv
013017
013017      string
013017         parm-cycle-date (5:2) '/'
013017         parm-cycle-date (7:2) '/'
013017         parm-cycle-date (1:4) delimited by size
013017              into ws-test-date
013017     end-string
013017
013017     move ws-test-date to ws-moe-date

013017      string
013017         parm-2-bank-days (5:2) '/'
013017         parm-2-bank-days (7:2) '/'
013017         parm-2-bank-days (1:4) delimited by size
013017              into ws-ach-cashed-date
013017     end-string
013017
052218     MOVE PARM-CYCLE-DATE        TO DC-GREG-DATE-CYMD
052218     MOVE 'L'                    TO DC-OPTION-CODE
052218     move +0                     to dc-elapsed-months
052218                                    dc-elapsed-days
052218     PERFORM 8500-DATE-CONVERSION
052218
052218     IF NO-CONVERSION-ERROR
052218        MOVE DC-BIN-DATE-1       TO WS-CHECK-WRITER-DATE
052218     ELSE
052218        DISPLAY ' INVALID PARM DATE ' PARM-CYCLE-DATE
052218        PERFORM ABEND-PGM
052218     END-IF

052218*    ACCEPT WS-JULIAN-YYDDD FROM DAY
DJNA  **   THESE LINES CAN BE USED TO RUN A PARALLEL TEST THAT WILL CAUSE
DJNA  **   THE DATES IN THE MICRDRFT FILE, THE ELTRLR, AND THE ELCHKQ FILE
DJNA  **   TO MATCH THE PRODUCTION FILES.
DJNA  **   MOVE 05077 TO WS-JULIAN-YYDDD *> JULIAN VERSION OF 03/18/2005
DJNA  **   MOVE 05097 TO WS-JULIAN-YYDDD *> JULIAN VERSION OF 04/07/2005
DJNA  **   MOVE 175538 TO WS-TIME
052218*    MOVE WS-JULIAN-YYDDD       TO DC-JULIAN-YYDDD
052218*    MOVE '5'                   TO DC-OPTION-CODE.
052218*    PERFORM 8500-DATE-CONVERSION.
052218*    MOVE DC-BIN-DATE-1         TO  WS-CHECK-WRITER-DATE.

           OPEN INPUT CONTROL-CARD

           READ CONTROL-CARD INTO EL352-INPUT
             AT END
               MOVE 'EMPTY CONTROL-CARD FILE' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-READ

           CLOSE CONTROL-CARD.

      *    NOTE *******************************************************
      *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *
      *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
      *         *******************************************************.

           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.

           MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS
                                           PI-CONTROL-GROUP (1)
                                           PI-CONTROL-GROUP (2)
                                           PI-CONTROL-GROUP (3)
                                           PI-CONTROL-GROUP (4).

           OPEN I-O MICR-DRAFT-FILE
           IF MICR-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              IF MICR-STATUS = '05'
                 DISPLAY 'MICRDRFT FILE DID NOT EXIST - FILE CREATED'
              ELSE
                 MOVE MICR-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'OPEN ERROR ON MICRDRFT FILE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           OPEN I-O ELCHKQ2
           IF CHKQ-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELCHKQ2 FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ELCNTL
           IF CNTL-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O ELTRLR
           IF TRLR-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ELCERT
           IF CERT-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE CERT-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELCERT FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ELMSTR
           IF MSTR-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE MSTR-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELMSTR FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ERACCT
           IF ACCT-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ERACCT FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           OPEN INPUT ELBENE
           IF BENE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE BENE-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'OPEN ERROR ON ELBENE FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF
042809
042809     OPEN INPUT ELCRTT
042809     IF ELCRTT-FILE-STATUS = '00' OR '97'
042809        CONTINUE
042809     ELSE
042809        MOVE ELCRTT-FILE-STATUS TO WS-ABEND-FILE-STATUS
042809        MOVE 'OPEN ERROR ON ELCRTT FILE' TO WS-ABEND-MESSAGE
042809        PERFORM ABEND-PGM
042809     END-IF
072110
072110     OPEN OUTPUT NAPERSOFT-LETTERS.

           IF (OPTION GREATER THAN ZERO AND
               OPTION LESS THAN '4')
             CONTINUE
           ELSE
             MOVE 'OPTION NOT VALID' TO WS-ABEND-MESSAGE
             PERFORM ABEND-PGM.

      *    NOTE *******************************************************
      *         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       *
      *         *  ENTERED.                                           *
      *         *******************************************************.

           SET PI-INDEX TO +1.

           PERFORM VARYING EL352A-INDEX FROM 1 BY 1
             UNTIL EL352A-INDEX > 4

             IF EL352A-CONTROL-GROUP-X (EL352A-INDEX) = SPACES
                 EXIT PERFORM CYCLE
             END-IF

             IF EL352A-CONTROL-GROUP (EL352A-INDEX) IS NOT NUMERIC
                 MOVE 'CONTROL GROUP IS NOT NUMERIC'
                   TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-IF

             MOVE EL352A-CONTROL-GROUP (EL352A-INDEX)
                                     TO  PI-CONTROL-GROUP (PI-INDEX)
             SET PI-INDEX UP BY +1

             IF PI-INDEX IS GREATER THAN +2
                         AND PI-CONTROL-GROUP (PI-INDEX - 2)
               NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)
                 MOVE 'CONTROL GROUPS MUST BE ENTERED IN ASCENDING '
                    & 'SEQUENCE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-IF

             MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE
             MOVE DTE-CLASIC-COMPANY-CD  TO  CQ-COMPANY-CD-A1
             MOVE EL352A-CONTROL-GROUP (EL352A-INDEX)
                                         TO  CQ-CONTROL-NUMBER-A1
             START ELCHKQ2 KEY IS NOT < CQ-CONTROL-BY-PAYEE
             EVALUATE CHKQ-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '23'
                 MOVE 'CONTROL GROUP NOT FOUND ON CHECK QUEUE'
                   TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
               WHEN OTHER
                 MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'START ERROR ON ELCHKQ2 FILE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-EVALUATE

             PERFORM UNTIL EXIT
               READ ELCHKQ2 NEXT RECORD
               IF CHKQ-STATUS = '00'
                  CONTINUE
               ELSE
                  MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                  MOVE 'READ NEXT ERROR ON ELCHKQ2 FILE'
                    TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF

               IF CQ-COMPANY-CD-A1 NOT = DTE-CLASIC-COMPANY-CD
               OR CQ-CONTROL-NUMBER-A1 NOT = EL352A-CONTROL-GROUP
                                                          (EL352A-INDEX)
                 MOVE 'THERE ARE NO VALID CHECKS TO BE PRINTED FOR THIS'
                    & ' CONTROL GROUP ' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
               END-IF

               IF CQ-ENTRY-TYPE NOT = 'Q'
                   EXIT PERFORM CYCLE
               END-IF

               IF OPTION = '2'
                 AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO
                   EXIT PERFORM
               END-IF

               IF OPTION = '3'
                 AND CQ-TIMES-PRINTED GREATER THAN ZERO
                   EXIT PERFORM
               END-IF

             END-PERFORM

           END-PERFORM

           IF OPTION = ('2' OR '3')
             AND PI-INDEX NOT GREATER THAN +1
               MOVE 'YOU SELECTED OPTION 2 OR 3 AND NO CONTROL GROUPS'
                  & ' WERE ENTERED' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-IF

           IF PI-INDEX GREATER THAN +1

             SET PI-INDEX DOWN BY +1
             SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX
             SET EL352A-INDEX TO +1

             PERFORM VARYING PI-INDEX FROM 1 BY 1 UNTIL PI-INDEX > 4
               IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO
                   MOVE PI-CONTROL-GROUP (PI-INDEX)
                               TO  EL352A-CONTROL-GROUP   (EL352A-INDEX)
               ELSE
                   MOVE SPACES TO  EL352A-CONTROL-GROUP-X (EL352A-INDEX)
               END-IF

               SET EL352A-INDEX UP BY +1
             END-PERFORM

           END-IF

           SET EL352A-INDEX
               PI-INDEX TO +1.

      *    NOTE *******************************************************
      *         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL. *
      *         *  NOW DO THE PRE-EDIT.                               *
      *         *                                                     *
      *         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    *
      *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
      *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
      *         *  STEPS:                                             *
      *         *                                                     *
      *         *  1. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       *
      *         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       *
      *         *                                                     *
      *         *  2. THAT ALL CHECKS HAVE A CHECK NUMBER ASSIGNED.   *
      *         *                                                     *
      *         *  3. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        *
      *         *                                                     *
      *         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A *
      *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          *
      *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   *
      *         *  STEPS:                                             *
      *         *                                                     *
      *         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  *
      *         *     BEEN PREVIOUSLY PRINTED.                        *
      *         *                                                     *
      *         *******************************************************.


           MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE

           MOVE DTE-CLASIC-COMPANY-CD  TO  CQ-COMPANY-CD-A1.

           START ELCHKQ2 KEY IS NOT < CQ-CONTROL-BY-PAYEE
           EVALUATE CHKQ-STATUS
             WHEN '00'
               CONTINUE
             WHEN '23'
               MOVE 'NO CHECKS TO BE PRINTED FOR THIS COMPANY'
                 TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
             WHEN OTHER
               MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'START ERROR ON ELCHKQ2 FILE' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-EVALUATE.

           PERFORM UNTIL EXIT
             READ ELCHKQ2 NEXT RECORD
             EVALUATE CHKQ-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '10'
                 EXIT PERFORM
               WHEN OTHER
                 MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'READ NEXT ERROR ON ELCHKQ2 FILE'
                   TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-EVALUATE

             IF CQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
                 EXIT PERFORM
             END-IF

             MOVE '1' TO PI-VALID-RCD-SW

             IF OPTION = '1'
      *        NOTE *******************************************************
      *             *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  *
      *             *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. *
      *             *******************************************************.

               IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP
                   MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP
                   MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED
               END-IF

               IF WS-TIMES-PRINTED GREATER THAN ZERO
                   EXIT PERFORM CYCLE
               END-IF
             END-IF

             IF (OPTION = '1')
             OR (OPTION = '2' AND
                (CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (1)
                                  OR PI-CONTROL-GROUP (2)
                                  OR PI-CONTROL-GROUP (3)
                                  OR PI-CONTROL-GROUP (4)))
               IF CQ-TIMES-PRINTED GREATER THAN ZERO
                  MOVE 'SOME OF THE CHECKS TO BE PRINTED ARE ALREADY'
                     & ' QUEUED TO PRINT' TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF

               IF CQ-CHECK-NUMBER = SPACES
                  MOVE 'NOT ALL CHECKS HAVE A NUMBER ASSIGNED'
                    TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF
             END-IF

             IF (OPTION = '3' AND
                (CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (1)
                                  OR PI-CONTROL-GROUP (2)
                                  OR PI-CONTROL-GROUP (3)
                                  OR PI-CONTROL-GROUP (4)))
               IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO
                  MOVE 'TRYING TO PRINT CHECKS THAT HAVE NOT BEEN '
                     & 'PREVIOUSLY PRINTED' TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF
             END-IF

           END-PERFORM.

           IF PI-VALID-RCD-SW NOT = '1'
               MOVE 'THERE ARE NO CHECKS ON THE FILE TO BE RELEASED - '
                  & 'NOTIFY LOGIC' TO WS-ABEND-MESSAGE
040312         display 'THERE ARE NO CHECKS ON  FILE TO BE RELEASED - '
040312         GO TO 9999-END-OF-JOB
040312*        PERFORM ABEND-PGM
           END-IF

      *    NOTE *******************************************************
      *         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  *
      *         *******************************************************.

           MOVE DTE-CLIENT             TO  CF-CONTROL-PRIMARY.
           MOVE '1'                    TO  CF-RECORD-TYPE.
           MOVE ZERO                   TO  CF-SEQUENCE-NO.

           READ ELCNTL KEY IS CF-CONTROL-PRIMARY
           IF CNTL-STATUS = '00'
              CONTINUE
           ELSE
              MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
              MOVE 'READ ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.
           MOVE CF-CURRENT-MONTH-END   TO  PI-MONTH-END-SAVE.

           MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE.
           MOVE DTE-CLASIC-COMPANY-CD  TO  CQ-COMPANY-CD-A1.

           IF OPTION = '1'
               START ELCHKQ2 KEY IS NOT < CQ-CONTROL-BY-PAYEE
               IF CHKQ-STATUS NOT = '00'
                  MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                  MOVE 'START ERROR ON ELCHKQ2 FILE' TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
               END-IF
               PERFORM 0325-MAIN-LOGIC
           ELSE
               PERFORM VARYING PI-INDEX FROM 1 BY 1
                 UNTIL PI-INDEX > PI-NUMBER-OF-CONTROL-GROUPS
                 MOVE PI-CONTROL-GROUP (PI-INDEX)
                                     TO CQ-CONTROL-NUMBER-A1
                 MOVE ZERO           TO CQ-SEQUENCE-NUMBER-A1

                 START ELCHKQ2 KEY EQUAL CQ-CONTROL-BY-NUMBER
                 IF CHKQ-STATUS NOT = '00'
                    MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                    MOVE 'START ERROR ON ELCHKQ2 FILE'
                      TO WS-ABEND-MESSAGE
                    PERFORM ABEND-PGM
                 END-IF
                 PERFORM 0325-MAIN-LOGIC
               END-PERFORM
           END-IF.

           DISPLAY DRAFTS-WRITTEN ' CHECKS SUCCESSFULLY PROCESSED'
           IF CHECKS-WITHOUT-ADDRESSES
             DISPLAY 'SOME OF THE CHECKS PRINTED DO NOT HAVE ADDRESSES'
           END-IF
           GO TO 9999-END-OF-JOB.


       0325-MAIN-LOGIC.
           PERFORM UNTIL EXIT
             READ ELCHKQ2 NEXT RECORD
             EVALUATE CHKQ-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '10'
                 EXIT PERFORM
               WHEN OTHER
                 MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'READ NEXT ERROR ON ELCHKQ2 FILE'
                   TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-EVALUATE

             IF CQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
                 EXIT PERFORM
             END-IF

             IF OPTION = '2' OR '3'
                 IF CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP (PI-INDEX)
                     EXIT PERFORM
                 END-IF
             END-IF

             IF CQ-ENTRY-TYPE NOT = 'Q'
                 EXIT PERFORM CYCLE
             END-IF

             IF ((OPTION = '1' OR '2') AND
                 CQ-TIMES-PRINTED GREATER THAN ZERO)
             OR (OPTION = '3' AND
                 CQ-TIMES-PRINTED NOT GREATER THAN ZERO)
                 EXIT PERFORM CYCLE
             END-IF

      *      NOTE *******************************************************
      *           *                PRINT THE CHECK                      *
      *           *******************************************************.

PEMTST       DISPLAY ' FOUND ONE ' CQ-CARRIER ' ' CQ-CLAIM-NO
             MOVE DTE-CLASIC-COMPANY-CD  TO  CL-COMPANY-CD
             MOVE CQ-CARRIER             TO  CL-CARRIER
                                             CPA-CARRIER
             MOVE CQ-CLAIM-NO            TO  CL-CLAIM-NO
                                             CPA-CLAIM-NO
             MOVE CQ-CERT-NO             TO  CL-CERT-NO
                                             CPA-CERT-NO
041813
041813       IF CQ-LAST-UPDATED-BY = +1750
041813           MOVE 'X'                TO  WS-SPECIAL-RELEASE-CHK
041813       ELSE
041813           MOVE ' '                TO  WS-SPECIAL-RELEASE-CHK
041813       END-IF

             IF CQ-CARRIER NOT = WS-LAST-CARRIER
                 PERFORM 1000-GET-CARRIER-NAME
                 MOVE CQ-CARRIER TO WS-LAST-CARRIER
             END-IF

      *      NOTE *******************************************************
      *           *            READ THE CLAIM MASTER RECORD             *
      *           *******************************************************.

             READ ELMSTR KEY IS CL-CONTROL-PRIMARY
             IF MSTR-STATUS = '00'
                CONTINUE
             ELSE
                MOVE MSTR-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'READ ERROR ON ELMSTR FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

      *      NOTE *******************************************************
      *           *       READ THE CERTIFICATE MASTER RECORD            *
      *           *******************************************************.

PEMTST       DISPLAY ' FOUND ELMSTR ' CL-CARRIER ' ' CL-CLAIM-NO

             MOVE DTE-CLASIC-COMPANY-CD  TO  CM-COMPANY-CD
             MOVE CL-CERT-CARRIER        TO  CM-CARRIER
             MOVE CL-CERT-GROUPING       TO  CM-GROUPING
             MOVE CL-CERT-STATE          TO  CM-STATE
             MOVE CL-CERT-ACCOUNT        TO  CM-ACCOUNT
             MOVE CL-CERT-EFF-DT         TO  CM-CERT-EFF-DT
             MOVE CL-CERT-NO             TO  CM-CERT-NO

             READ ELCERT KEY IS CM-CONTROL-PRIMARY
             IF CERT-STATUS = '00'
                CONTINUE
             ELSE
                MOVE CERT-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'READ ERROR ON ELCERT FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

PEMTST       DISPLAY ' FOUND ELCERT ' CM-CARRIER ' ' CM-CERT-NO
042809
042809*      NOTE *******************************************************
042809*           *       READ THE CERTIFICATE TRAILER RECORD           *
042809*           *******************************************************.
042809
042809       MOVE SPACES           TO WS-BIU-VIN
042809
042809       IF DTE-CLIENT = 'DCC' AND
042809          CM-ACCOUNT (9:2) = 'BI'
042809
042809           MOVE CM-CONTROL-PRIMARY    TO  WS-ELCRTT-PRIMARY
042809           MOVE 'C'                   TO  WS-ELCRTT-REC-TYPE
042809           MOVE WS-ELCRTT-KEY         TO  CS-CONTROL-PRIMARY
042809
042809           READ ELCRTT KEY IS CS-CONTROL-PRIMARY
042809           IF ELCRTT-FILE-STATUS = '00' 
042809               MOVE CS-VIN-NUMBER         TO WS-BIU-VIN
042809           ELSE
042809               DISPLAY 'CERT NUMBER ' CM-CERT-NO 
042809                  ' - READ ERROR ON ELCRTT FILE - STATUS = '
042809                  ELCRTT-FILE-STATUS  
042809           END-IF
042809       END-IF

      *      NOTE *******************************************************
      *           *          READ THE ACCOUNT MASTER RECORD             *
      *           *******************************************************.

             MOVE DTE-CLASIC-COMPANY-CD  TO  AM-COMPANY-CD

             MOVE CL-CERT-CARRIER        TO  AM-CARRIER
             MOVE CL-CERT-GROUPING       TO  AM-GROUPING
             MOVE CL-CERT-STATE          TO  AM-STATE
             MOVE CL-CERT-ACCOUNT        TO  AM-ACCOUNT
             MOVE CL-CERT-EFF-DT         TO  AM-EXPIRATION-DT

             MOVE SPACES                 TO  WS-ACCOUNT-HOLD-RECORD

             START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY
             EVALUATE ACCT-STATUS
               WHEN '00'
                 CONTINUE
               WHEN '23'
                 PERFORM 0340-MAIN-LOGIC
                 EXIT PERFORM
               WHEN OTHER
                 MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
                 MOVE 'START ERROR ON ERACCT FILE' TO WS-ABEND-MESSAGE
                 PERFORM ABEND-PGM
             END-EVALUATE

             PERFORM UNTIL EXIT
               READ ERACCT NEXT RECORD
               EVALUATE ACCT-STATUS
                 WHEN '00'
                   CONTINUE
                 WHEN '10'
                   PERFORM 0340-MAIN-LOGIC
                   EXIT PERFORM
                 WHEN OTHER
                   MOVE ACCT-STATUS TO WS-ABEND-FILE-STATUS
                   MOVE 'READ NEXT ERROR ON ERACCT FILE'
                     TO WS-ABEND-MESSAGE
                   PERFORM ABEND-PGM
               END-EVALUATE

               IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD OR
                  AM-CARRIER    NOT = CL-CERT-CARRIER   OR
                  AM-GROUPING   NOT = CL-CERT-GROUPING  OR
                  AM-STATE      NOT = CL-CERT-STATE     OR
                  AM-ACCOUNT    NOT = CL-CERT-ACCOUNT
                  IF WS-ACCOUNT-HOLD-RECORD = SPACES
                     PERFORM 0340-MAIN-LOGIC
                     EXIT PERFORM
                  ELSE
                     MOVE WS-ACCOUNT-HOLD-RECORD TO ACCOUNT-MASTER
                     EXIT PERFORM
                  END-IF
               END-IF

               IF AM-EXPIRATION-DT = HIGH-VALUES
                   EXIT PERFORM
               ELSE
                   MOVE ACCOUNT-MASTER     TO  WS-ACCOUNT-HOLD-RECORD
               END-IF

             END-PERFORM

PEMTST       DISPLAY ' FOUND ERACCT ' CM-CARRIER ' ' CM-CERT-NO
042809
042809       MOVE 'N' TO WS-NAME-END-FOUND
042809       MOVE AM-NAME              TO WS-ACCOUNT-NAME
082219                                    ext-account-name
042809       PERFORM VARYING WS-SUB FROM 30 BY -1
042809          UNTIL NAME-END-FOUND OR WS-SUB = 0
042809           IF WS-ACCT-NAME (WS-SUB) GREATER THAN SPACES
042809               MOVE 'Y' TO WS-NAME-END-FOUND
042809           END-IF
042809       END-PERFORM
042809       COMPUTE WS-ACCOUNT-NAME-LENGTH = WS-SUB + 1

             MOVE SPACES                 TO  CHECK-PASS-AREA

             MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY
             MOVE CQ-PMT-TRLR-SEQUENCE   TO  AT-SEQUENCE-NO

             READ ELTRLR KEY IS AT-CONTROL-PRIMARY
             IF TRLR-STATUS = '00'
                CONTINUE
             ELSE
                MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'READ ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

             MOVE ZERO                   TO  CPA-ALIGNMENT

             MOVE CM-CARRIER             TO  CPA-CARRIER
             MOVE CM-GROUPING            TO  CPA-GROUP

080305       IF DTE-CLIENT NOT = 'DCC'
                MOVE CM-ACCOUNT          TO  CPA-ACCOUNT
080305       END-IF

             MOVE CM-STATE               TO  CPA-STATE
             MOVE CM-CERT-NO             TO  CPA-CERT-NO
             MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE
             MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT

             MOVE CL-CLAIM-NO            TO  CPA-CLAIM-NO
             MOVE CL-CLAIM-STATUS        TO  CPA-CLAIM-STATUS
             MOVE CL-LAST-CLOSE-REASON   TO  CPA-LAST-CLOSE-REASON

             PERFORM 5000-MOVE-NAME
             MOVE WS-NAME-WORK           TO  CPA-INSURED-NAME

             MOVE CL-CLAIM-TYPE          TO  CPA-CLAIM-TYPE
             MOVE AT-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE
             MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY

             MOVE AT-PAYMENT-NOTE-SEQ-NO TO WS-PAYMENT-NOTE-SEQ-NO
072110       MOVE AT-PRINT-EOB-WITH-CHECK TO WRK-PRINT-EOB-IND
020413       MOVE AT-PRINT-CLM-FORM      TO WRK-PRINT-CLM-FORM
020413       MOVE AT-PRINT-SURVEY        TO WRK-PRINT-SURVEY
102413       IF AT-SPECIAL-RELEASE = 'Y'
102413           MOVE 'X'                TO WS-SPECIAL-RELEASE-CHK
102413       END-IF

PEMTMP*      IF CQ-CHECK-NUMBER (1:1) = 'T'
PEMTMP*         MOVE '8'                 TO CQ-CHECK-NUMBER (1:1)
PEMTMP*      END-IF
             MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER
                                             AT-CHECK-NO

             MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID
             MOVE CL-TOTAL-PAID-AMT      TO  CPA-AMOUNT-PAID-TO-DATE
             MOVE AT-DAYS-IN-PERIOD      TO  CPA-DAYS-PAID
             MOVE AT-DAILY-RATE          TO  CPA-DAILY-RATE
             MOVE AT-ELIMINATION-DAYS    TO  CPA-ELIMINATION-DAYS
             MOVE AT-BENEFIT-TYPE        TO  CPA-BENEFIT-TYPE
             MOVE AT-EXPENSE-TYPE        TO  CPA-EXPENSE-TYPE
             MOVE CL-NO-OF-PMTS-MADE     TO  CPA-NO-OF-PMTS-MADE
             MOVE CL-PROCESSOR-ID        TO  CPA-EXAMINER
             MOVE AT-PAYMENT-ORIGIN      TO  CPA-PAYMENT-ORIGIN
             MOVE CL-CCN-A5              TO  CPA-CREDIT-CARD-NO

013017       if at-ach-payment = 'Y'
013017          move 'Y'               to cpa-ach-payment
013017       else
013017          move 'N'               to cpa-ach-payment
013017       end-if

             IF AT-PAYMENT-ORIGIN = '2'
                 MOVE 'Y'                TO  WS-AUTO-PAY-SW
             ELSE
                 MOVE 'N'                TO  WS-AUTO-PAY-SW
             END-IF

      *      NOTE *******************************************************
      *           *      CLAIM TYPE      MEANING                        *
      *           *          1         DEATH CLAIM (INDIVIDUAL)         *
      *           *          2         DISABILITY CLAIM (INDIVIDUAL)    *
      *           *          3         OUTSTANDING BALANCE (DEATH)      *
      *           *          4         OUTSTANDING BALANCE (DISABILITY) *
      *           *******************************************************.

100518       IF CL-CLAIM-TYPE = 'L' OR 'O'
                 IF CL-CLAIM-PREM-TYPE = '2'
                     MOVE '3'            TO  CPA-CLAIM-CODE
                 ELSE
                     MOVE '1'            TO  CPA-CLAIM-CODE
                 END-IF
             ELSE
                 IF CL-CLAIM-PREM-TYPE = '2'
                     MOVE '4'            TO  CPA-CLAIM-CODE
                 ELSE
                     MOVE '2'            TO  CPA-CLAIM-CODE
                 END-IF
             END-IF

      *      NOTE *******************************************************
      *           *      PAY CODE       MEANING                         *
      *           *         A        ADDITIONAL DEATH CLAIM             *
      *           *         P        PARTIAL PAYMENT                    *
      *           *         F        FINAL PAYMENT                      *
      *           *         S        LUMP SUM DISABILITY                *
      *           *******************************************************.

             MOVE AT-PAYMENT-TYPE        TO  CPA-PAY-CODE
                                             WS-PAYMENT-TYPE
             INSPECT CPA-PAY-CODE CONVERTING '123456789' TO 'PFSA     '

             MOVE CL-INCURRED-DT         TO  CPA-INCURRED-DT
             MOVE CL-REPORTED-DT         TO  CPA-REPORTED-DT

             IF NOT DTE-CLAIM-PAID-THRU-TO = '1'
                MOVE AT-PAID-THRU-DT     TO  CPA-PAID-THRU-DT
             ELSE
                MOVE AT-PAID-THRU-DT     TO  DC-BIN-DATE-1
                MOVE +1                  TO  DC-ELAPSED-DAYS
                MOVE +0                  TO  DC-ELAPSED-MONTHS
                MOVE '6'                 TO  DC-OPTION-CODE
                PERFORM 8500-DATE-CONVERSION
                IF NO-CONVERSION-ERROR
                   MOVE DC-BIN-DATE-2    TO  CPA-PAID-THRU-DT
                END-IF
             END-IF
072110       MOVE AT-PREV-PAID-THRU-DT    TO DC-BIN-DATE-1
072110       MOVE SPACES                  TO DC-OPTION-CODE
072110       PERFORM 8500-DATE-CONVERSION
072110       IF DC-ERROR-CODE EQUAL TO SPACES
072110           MOVE DC-GREG-DATE-A-EDIT TO WRK-PREV-PAID-THRU
072110       END-IF

             MOVE AT-PAID-FROM-DT        TO  CPA-PAID-FROM-DT
             MOVE CL-LAST-PMT-DT         TO  CPA-PAID-DT

             MOVE WS-CARRIER-ADDRESS-DATA  TO  CPA-CARRIER-ADDRESS-DATA

             MOVE AM-NAME                TO  CPA-ACCOUNT-NAME
             MOVE AM-PERSON              TO  CPA-ACCOUNT-IN-CARE-OF
             MOVE AM-ADDRS               TO  CPA-ACCOUNT-ADDRESS-LINE1
             MOVE SPACES                 TO  CPA-ACCOUNT-ADDRESS-LINE2
051810       MOVE SPACES                 TO  CPA-ACCOUNT-CITY-ST
072110*051810       STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
072110*051810          DELIMITED BY '  ' INTO CPA-ACCOUNT-CITY-ST
072110*051810       END-STRING
072110       MOVE AM-ADDR-CITY           TO  CPA-ACCOUNT-CITY
072110       MOVE AM-ADDR-STATE          TO  CPA-ACCOUNT-STATE
             MOVE AM-ZIP                 TO  CPA-ACCOUNT-ZIP-CODE
             MOVE AM-TEL-NO              TO  WS-WORK-PHONE
             INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'
             MOVE WS-NUMERIC-PHONE       TO  CPA-ACCOUNT-PHONE-NO

             IF CM-SSN-STATE   = CM-STATE AND
                CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
                 MOVE SPACES             TO  CPA-SOC-SEC-NO
             ELSE
                 MOVE CM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO
             END-IF

             MOVE CM-STATE               TO  WS-SSN-STATE
                                             WS-MEMBER-STATE
             MOVE CM-ACCOUNT             TO  WS-SSN-ACCOUNT
                                             WS-MEMBER-ACCOUNT
             MOVE CM-INSURED-LAST-NAME   TO  WS-SSN-LN3
                                             WS-MEMBER-LN4

             IF CM-MEMB-STATE   = CM-STATE AND
                CM-MEMB-ACCOUNT = CM-ACCOUNT-PRIME
                 MOVE SPACES             TO  CPA-MEMBER-NUMBER
             ELSE
                 MOVE CM-MEMBER-NO       TO  CPA-MEMBER-NUMBER
             END-IF

             MOVE AT-PAYEE-TYPE-CD       TO  WS-PAYEE-CODE
                                             CPA-PAYEE-TYPE-CD

             MOVE WS-CHECK-WRITER-DATE   TO  AT-CHECK-WRITTEN-DT
                                             CQ-CHECK-WRITTEN-DT
                                             CPA-CHECK-DATE

             IF OPTION NOT = '3'
                 MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT
             END-IF

             MOVE +1760                  TO  CQ-LAST-UPDATED-BY
             MOVE WS-TIME                TO  CQ-LAST-UPDATED-HHMMSS
             MOVE WS-JULIAN-YYDDD        TO  DC-JULIAN-YYDDD
             MOVE '5'                    TO  DC-OPTION-CODE
             PERFORM 8500-DATE-CONVERSION
             MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT

             REWRITE ACTIVITY-TRAILERS
             IF TRLR-STATUS NOT = '00'
                MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'REWRITE ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

             IF WS-PAYMENT-NOTE-SEQ-NO GREATER THAN +0 AND
                WS-PAYMENT-NOTE-SEQ-NO LESS THAN +4096
                MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY
                MOVE WS-PAYMENT-NOTE-SEQ-NO TO  AT-SEQUENCE-NO
                READ ELTRLR KEY IS AT-CONTROL-PRIMARY
                EVALUATE TRLR-STATUS
                  WHEN '00'
                    IF AT-TRAILER-TYPE = '6'
                       MOVE AT-INFO-LINE-1 TO CPA-COMMENT
                       MOVE AT-INFO-LINE-2 TO CPA-COMMENT-2
072110                 MOVE AT-EOB-CODES-EXIST TO WRK-NOTE-CODE-IND
                    END-IF
                  WHEN '23'
                    CONTINUE
                  WHEN OTHER
                    MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
                    MOVE 'READ ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
                    PERFORM ABEND-PGM
                END-EVALUATE
             END-IF

             IF WS-AUTO-PAY-SW = 'Y'
               MOVE CL-CONTROL-PRIMARY         TO  AT-CONTROL-PRIMARY
               MOVE CL-AUTO-PAY-SEQ            TO  AT-SEQUENCE-NO

               READ ELTRLR KEY IS AT-CONTROL-PRIMARY
               EVALUATE TRLR-STATUS
                 WHEN '00'
                   IF AT-TRAILER-TYPE = '3'
                     MOVE AT-SCHEDULE-END-DT   TO  CPA-AUTO-PAY-END-DT
                   END-IF
                 WHEN '23'
                   CONTINUE
                 WHEN OTHER
                   MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
                   MOVE 'READ ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
                   PERFORM ABEND-PGM
               END-EVALUATE
             END-IF
072110
072110       MOVE SPACES TO WRK-AUTO-PAY-END-DT
072110       IF CPA-AUTO-PAY-END-DT NOT EQUAL LOW-VALUES AND SPACES
072110           MOVE CPA-AUTO-PAY-END-DT     TO DC-BIN-DATE-1
072110           MOVE SPACES                  TO DC-OPTION-CODE
072110           PERFORM 8500-DATE-CONVERSION
072110           IF DC-ERROR-CODE EQUAL TO SPACES
072110               MOVE DC-GREG-DATE-A-EDIT TO WRK-AUTO-PAY-END-DT
072110           END-IF
072110       END-IF

             IF CL-ACCOUNT-ADDR-CNT NOT = ZERO
               MOVE CL-ACCOUNT-ADDR-CNT TO AT-SEQUENCE-NO
               ADD +20    TO AT-SEQUENCE-NO
               PERFORM 0360-MAIN-LOGIC
               MOVE CPA-PAYEE-NAME          TO CPA-ACCOUNT-NAME
               MOVE CPA-PAYEE-ADDRESS-LINE2 TO CPA-ACCOUNT-ADDRESS-LINE1
               MOVE CPA-PAYEE-ADDRESS-LINE3 TO CPA-ACCOUNT-ADDRESS-LINE2
               MOVE CPA-PAYEE-CITY-STATE    TO CPA-ACCOUNT-CITY-ST
               MOVE CPA-PAYEE-ZIP           TO CPA-ACCOUNT-ZIP-CODE
             END-IF

062821       IF DTE-CLIENT = 'CID' OR 'DCC' OR 'AHL' OR 'FNL'
                MOVE +91                 TO AT-SEQUENCE-NO
                READ ELTRLR KEY IS AT-CONTROL-PRIMARY
                IF TRLR-STATUS = '00'
                   MOVE AT-INFO-LINE-1   TO CPA-BENEFICIARY
                ELSE
                   MOVE SPACES           TO CPA-BENEFICIARY
                END-IF
             END-IF

             MOVE SPACES               TO WS-LOAN-NUMBER
             
             IF (DTE-CLIENT = 'DCC')
                AND (CPA-BENEFICIARY NOT = SPACES)
                MOVE CPA-BENEFICIARY   TO WS-WORK-FIELD1
                MOVE +10               TO S2
                PERFORM VARYING S1 FROM +25 BY -1 UNTIL
                   (S1 < +1)
                   OR (S2 < +1)
                   IF WS-WORK-FLD1 (S1) NOT = SPACE
                      MOVE WS-WORK-FLD1 (S1) TO WS-WORK-FLD2 (S2)
                      SUBTRACT +1 FROM S2
                   END-IF
                END-PERFORM
080305          MOVE WS-LOAN-NUMBER      TO  CPA-ACCOUNT
             END-IF
             
             MOVE CL-INSURED-ADDR-CNT TO AT-SEQUENCE-NO

             PERFORM 0360-MAIN-LOGIC
             
             MOVE CPA-PAYEE-NAME          TO CPA-INSURED-ADDR-TRLR-NAME
             MOVE CPA-PAYEE-ADDRESS-LINE1 TO CPA-INSURED-ADDRESS-LINE1
             MOVE CPA-PAYEE-ADDRESS-LINE2 TO CPA-INSURED-ADDRESS-LINE2
             MOVE CPA-PAYEE-ADDRESS-LINE3 TO CPA-INSURED-ADDRESS-LINE3
             MOVE CPA-PAYEE-CITY-STATE    TO CPA-INSURED-CITY-STATE
             MOVE CPA-PAYEE-ZIP           TO CPA-INSURED-ZIP

             EVALUATE TRUE
               WHEN WS-PAYEE-CD = 'I'
052813           MOVE WS-PAYEE-SEQ-NUM TO AT-SEQUENCE-NO
052813           PERFORM 0360-MAIN-LOGIC

               WHEN WS-PAYEE-CD = 'B'
                 IF (CL-BENIF-ADDR-CNT = +0) OR
                    (WS-PAYEE-SEQ-NUM = 0 OR 9)
                   MOVE CL-BENEFICIARY     TO  BE-BENEFICIARY
                   PERFORM 2000-GET-BENEFICIARY
                 ELSE
                   MOVE WS-PAYEE-SEQ-NUM   TO  AT-SEQUENCE-NO
                   ADD +10                 TO  AT-SEQUENCE-NO
                   PERFORM 0360-MAIN-LOGIC
                 END-IF

               WHEN WS-PAYEE-CD = 'O'
                 MOVE WS-PAYEE-SEQ-NUM      TO  AT-SEQUENCE-NO
                 ADD +50                    TO  AT-SEQUENCE-NO
                 PERFORM 0360-MAIN-LOGIC

               WHEN WS-PAYEE-CD = 'Q'
                 MOVE WS-PAYEE-SEQ-NUM      TO  AT-SEQUENCE-NO
                 ADD +60                    TO  AT-SEQUENCE-NO
                 PERFORM 0360-MAIN-LOGIC

               WHEN WS-PAYEE-CD = 'P'
                 MOVE WS-PAYEE-SEQ-NUM      TO  AT-SEQUENCE-NO
                 ADD +30                    TO  AT-SEQUENCE-NO
                 PERFORM 0360-MAIN-LOGIC

               WHEN WS-PAYEE-CD = 'E'
                 MOVE WS-PAYEE-SEQ-NUM      TO  AT-SEQUENCE-NO
                 ADD +40                    TO  AT-SEQUENCE-NO
                 PERFORM 0360-MAIN-LOGIC

               WHEN (WS-PAYEE-CD = 'A') AND
                    (CL-ACCOUNT-ADDR-CNT NOT = +0) AND
                    (WS-PAYEE-SEQ-NUM GREATER THAN 0)
                 MOVE WS-PAYEE-SEQ-NUM     TO  AT-SEQUENCE-NO
                 ADD +20                   TO  AT-SEQUENCE-NO
                 PERFORM 0360-MAIN-LOGIC

               WHEN OTHER
                 MOVE AM-NAME           TO  CPA-PAYEE-NAME
                 MOVE AM-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1
                 MOVE AM-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2
                 MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3
051810           MOVE SPACES            TO  CPA-PAYEE-CITY-STATE
072110*051810           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
072110*051810              DELIMITED BY '  ' INTO CPA-PAYEE-CITY-STATE
072110*051810           END-STRING
072110           MOVE AM-ADDR-CITY      TO  CPA-PAYEE-CITY
072110           MOVE AM-ADDR-STATE     TO  CPA-PAYEE-STATE
                 MOVE AM-ZIP            TO  CPA-PAYEE-ZIP

             END-EVALUATE

             IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
                 MOVE WS-JULIAN-YYDDD    TO  DC-JULIAN-YYDDD
                 MOVE '5'                TO  DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-2
                 MOVE '1'                TO  DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING CPA-INSURED-AGE
             ELSE
                 MOVE ZERO               TO  CPA-INSURED-AGE
             END-IF

052614       IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
022122          OR 'B' OR 'H'
                 MOVE DTE-CLIENT         TO  CF-COMPANY-ID
                 MOVE '5'                TO  CF-RECORD-TYPE
                 MOVE CM-AH-BENEFIT-CD   TO  CF-HI-BEN-IN-REC
032311           MOVE CM-AH-BENEFIT-CD   TO  WRK-HOLD-BENEFIT-CD
072110           MOVE CM-AH-BENEFIT-AMT  TO  WRK-MO-BENEFIT
072110           MOVE CM-AH-LOAN-EXPIRE-DT TO WRK-HOLD-EXP-DT
                 PERFORM 8700-LOCATE-BENEFIT
                 IF WS-KIND NOT = SPACES
                    MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE
                    MOVE WS-RETRO-ELIM      TO  CPA-BENEFIT-TYPE
                    MOVE WS-RETRO-DAYS      TO  CPA-ELIMINATION-DAYS
                    MOVE CM-AH-BENEFIT-AMT  TO  CPA-MONTHLY-BENEFIT
                 ELSE
                    MOVE ZERO               TO  CPA-ELIMINATION-DAYS
                                                CPA-MONTHLY-BENEFIT
                 END-IF
             ELSE
                 MOVE ZERO               TO  CPA-ELIMINATION-DAYS
                                             CPA-MONTHLY-BENEFIT
                 MOVE DTE-CLIENT         TO  CF-COMPANY-ID
                 MOVE '4'                TO  CF-RECORD-TYPE
                 MOVE CM-LF-BENEFIT-CD   TO  CF-HI-BEN-IN-REC
032311           MOVE CM-LF-BENEFIT-CD   TO  WRK-HOLD-BENEFIT-CD
072110           MOVE ZERO               TO  WRK-MO-BENEFIT
072110           MOVE CM-LF-LOAN-EXPIRE-DT TO WRK-HOLD-EXP-DT
                 PERFORM 8700-LOCATE-BENEFIT
                 IF WS-KIND NOT = SPACES
                    MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE
                 END-IF
             END-IF
072110       MOVE WRK-HOLD-EXP-DT         TO DC-BIN-DATE-1
072110       MOVE SPACES                  TO DC-OPTION-CODE
072110       PERFORM 8500-DATE-CONVERSION
072110       IF DC-ERROR-CODE EQUAL TO SPACES
072110           MOVE DC-GREG-DATE-A-EDIT TO WRK-CERT-EXP-DT
072110       END-IF

             IF CM-CERT-EFF-DT NOT = LOW-VALUES
               MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1

052614         IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
022122            OR 'B' OR 'H'
                   MOVE CM-AH-ORIG-TERM    TO  DC-ELAPSED-MONTHS
                   MOVE CM-AH-BENEFIT-CD   TO  CPA-BENEFIT-CD
               ELSE
                   MOVE CM-LF-BENEFIT-AMT  TO  CPA-TOTAL-BENEFIT
                   MOVE CM-LF-BENEFIT-CD   TO  CPA-BENEFIT-CD
                   MOVE CM-LF-ORIG-TERM    TO  DC-ELAPSED-MONTHS
               END-IF

               MOVE ZERO                   TO  DC-ELAPSED-DAYS
               MOVE '6'                    TO  DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION

               MOVE DC-BIN-DATE-2          TO  CPA-EXPIRE-DT
             END-IF

             ADD +1  TO  CQ-TIMES-PRINTED

      *      This line of code looks like a coding error in program EL176.
      *      However, it is necessary (at the least to achieve a parallel).
             MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT

             MOVE +1760                  TO  CQ-LAST-UPDATED-BY
             MOVE WS-TIME                TO  CQ-LAST-UPDATED-HHMMSS
             MOVE WS-JULIAN-YYDDD        TO  DC-JULIAN-YYDDD
             MOVE '5'                    TO  DC-OPTION-CODE
             PERFORM 8500-DATE-CONVERSION
             MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT
	
             REWRITE CHECK-QUE
             IF CHKQ-STATUS NOT = '00'
                MOVE CHKQ-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'REWRITE ERROR ON ELCHKQ2 FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

             MOVE CHECK-PRINT-LINES-SAVE-AREA TO CSO-CHECK-PRINT-LINES

      *      THE FOLLOWING IF IS USED TO PRINT OR SPACE THE FINAL MESSAGE
             IF (CPA-PAYMENT-TYPE = '2') AND
022122          (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F' OR
022122           'B' OR 'H')
                  MOVE WS-LINE-9  TO CSO9-FINAL-MESS
                  MOVE WS-LINE-10 TO CSO10-FINAL-MESS
                  MOVE WS-LINE-11 TO CSO11-FINAL-MESS
             ELSE
                  MOVE SPACES TO CSO9-FINAL-MESS
                                 CSO10-FINAL-MESS
                                 CSO11-FINAL-MESS
             END-IF

             MOVE WS-JULIAN-YYDDD   TO DC-JULIAN-YYDDD
             MOVE '5'               TO DC-OPTION-CODE
             PERFORM 8500-DATE-CONVERSION
             MOVE DC-BIN-DATE-1       TO WS-BIN-CURRENT-DT
             MOVE DC-GREG-DATE-1-EDIT TO WS-EDT-CURRENT-DT
030906       MOVE 'N' TO WS-IND-PAST-PMT
052614       IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F')
022122           AND (CPA-PAYMENT-TYPE = '1' OR '2' OR '3'
022122          OR 'B' OR 'H')
                 MOVE CPA-PAID-THRU-DT TO DC-BIN-DATE-1
                 MOVE +1               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
                 MOVE '6' TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 IF DC-BIN-DATE-2 > WS-BIN-CURRENT-DT
                    MOVE DC-GREG-DATE-1-EDIT TO CSO23-REPLY-DT
                                                WS-DCC-REPLY-DT
                 ELSE
030906              MOVE 'Y' TO WS-IND-PAST-PMT
                    MOVE WS-EDT-CURRENT-DT   TO CSO23-REPLY-DT
                                                WS-DCC-REPLY-DT
                 END-IF
             ELSE
                 MOVE SPACES           TO CSO23-REPLY-DT
                                          WS-DCC-REPLY-DT
             END-IF

             MOVE CPA-SOC-SEC-NO TO WS-SOC-SEC-NO
062821       IF DTE-CLIENT = 'CID' OR 'DCC' OR 'AHL' OR 'FNL'
                 MOVE SPACES TO CSO31-CC-ACCT
                                CSO31-CC-ACCT-NUMBER
                                CSO7-CC-ACCT
                                CSO7-CC-ACCT-NUMBER
             ELSE
                 MOVE 'ACCT # ' TO CSO31-CC-ACCT, CSO7-CC-ACCT
                 MOVE WS-SOC-SEC-NO TO CSO31-CC-ACCT-NUMBER
                                       CSO7-CC-ACCT-NUMBER
             END-IF

100518       IF (CPA-CLAIM-TYPE = 'L' OR 'O') AND CPA-PAYMENT-TYPE = '4'
                MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
                MOVE 'F'                TO  CSO5-PAYMENT-TYPE
             ELSE
                IF (CPA-PAYMENT-TYPE EQUAL TO '2')
                            OR
                   ((CPA-CLAIM-TYPE = 'G')
                   AND (CPA-PAYMENT-TYPE = '3'))
                   MOVE '  FINAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
                   MOVE 'F'                TO  CSO5-PAYMENT-TYPE
                ELSE
                   IF CPA-PAYMENT-TYPE = '3'
                      MOVE '  SETTLEMENT   '  TO  CSO7-TYPE-MESSAGE
                      MOVE 'F'                TO  CSO5-PAYMENT-TYPE
                   ELSE
                      MOVE 'PARTIAL PAYMENT'  TO  CSO7-TYPE-MESSAGE
                      MOVE 'P'                TO  CSO5-PAYMENT-TYPE
                   END-IF
                END-IF
             END-IF

             MOVE CPA-INSURED-ADDR-TRLR-NAME TO CSO11-MEMBER-NAME
             MOVE CPA-INSURED-NAME           TO CSO32-MEMBER-NAME
             MOVE CPA-INSURED-ADDRESS-LINE1  TO CSO12-MEMBER-ADDRESS1
             MOVE SPACES                     TO CSO5-PLAN-CODE

             MOVE CPA-INSURED-ADDRESS-LINE2  TO CSO13-MEMBER-ADDRESS2

             MOVE CPA-INSURED-ADDRESS-LINE3  TO CSO14-MEMBER-ADDRESS3
             MOVE CPA-CLAIM-NO           TO  CSO5-CLAIM-NO
                                             CSO31-CLAIM-NO
             MOVE CPA-CERT-NO            TO  CSO5-CERT-NO

             MOVE CPA-ACCOUNT            TO  CSO5-ACCT-NO

             MOVE CPA-INSURED-CITY-STATE  TO  CSO15-MEMBER-ADDRESS4

             IF CPA-INSURED-ZIP-CODE NOT EQUAL TO ZERO
                 MOVE CPA-INSURED-ZIP-CODE  TO  CSO16-MEMBER-ZIP-CODE
             END-IF

             IF CSO15-MEMBER-ADDRESS4 EQUAL TO SPACES
                 MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
                 MOVE SPACES                TO CSO16-MEMBER-ADDRESS5
             END-IF

             IF CSO14-MEMBER-ADDRESS3 EQUAL TO SPACES
                 MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
                 MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
                 MOVE SPACES                TO CSO16-MEMBER-ADDRESS5
             END-IF

             IF CSO13-MEMBER-ADDRESS2 EQUAL TO SPACES
                 MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
                 MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
                 MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
                 MOVE SPACES                TO CSO16-MEMBER-ADDRESS5
             END-IF

             IF CSO12-MEMBER-ADDRESS1 EQUAL TO SPACES
                 MOVE CSO13-MEMBER-ADDRESS2 TO CSO12-MEMBER-ADDRESS1
                 MOVE CSO14-MEMBER-ADDRESS3 TO CSO13-MEMBER-ADDRESS2
                 MOVE CSO15-MEMBER-ADDRESS4 TO CSO14-MEMBER-ADDRESS3
                 MOVE CSO16-MEMBER-ADDRESS5 TO CSO15-MEMBER-ADDRESS4
                 MOVE SPACES                TO CSO16-MEMBER-ADDRESS5
             END-IF

             IF CPA-PAID-FROM-DT NOT EQUAL TO LOW-VALUES
                 MOVE CPA-PAID-FROM-DT   TO  DC-BIN-DATE-1
                 MOVE SPACES             TO  DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 IF DC-ERROR-CODE EQUAL TO SPACES
                     MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-FROM-DATE
072110               MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-FROM-DT
                 END-IF
             END-IF

             IF CPA-PAID-THRU-DT NOT EQUAL TO LOW-VALUES
                 MOVE CPA-PAID-THRU-DT   TO  DC-BIN-DATE-1
                 MOVE SPACES             TO  DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 IF DC-ERROR-CODE EQUAL TO SPACES
                     MOVE DC-GREG-DATE-1-EDIT  TO  CSO5-PAID-THRU-DATE
                     MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-THRU-DT
                 END-IF

072110           IF WS-AUTO-PAY-SW = 'Y' AND
072110             CPA-AUTO-PAY-END-DT NOT EQUAL LOW-VALUES AND SPACES
072110               MOVE CPA-AUTO-PAY-END-DT TO DC-BIN-DATE-1
072110           ELSE
072110               MOVE CPA-PAID-THRU-DT TO DC-BIN-DATE-1
072110           END-IF
072110           MOVE +0               TO DC-ELAPSED-MONTHS
072110           MOVE +30              TO DC-ELAPSED-DAYS
072110           MOVE '6'              TO DC-OPTION-CODE
072110           PERFORM 8500-DATE-CONVERSION
072110           IF DC-ERROR-CODE EQUAL TO SPACES
072110              MOVE DC-GREG-DATE-A-EDIT TO WRK-FORM-DUE-DT
072110           END-IF
             END-IF             

             IF CPA-CLAIM-TYPE = 'G'
                MOVE SPACES            TO CSO5-PAID-FROM-DATE
                                          CSO5-PAID-THRU-DATE
             END-IF
             
             MOVE CPA-CERT-NO            TO WS-CHECK-AREA

062006       MOVE 01-HEADING           TO WS-COMPANY-NAME
062006       MOVE 01-COMP-NAME         TO WS-COMPANY-NAME2

             MOVE WS-COMPANY-NAME TO CSO2-COMPANY-NAME
             MOVE WS-COMPANY-NAME2 TO CSO26-COMPANY-NAME

             EVALUATE TRUE
             WHEN CPA-CLAIM-TYPE = 'A'
062821            AND (DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL')
                 MOVE 'A&H ' TO CSO5-PLAN-CODE

             WHEN CPA-CLAIM-TYPE = 'L'
062821            AND (DTE-CLIENT = 'CID' OR 'AHL' OR 'FNL')
                 MOVE 'LIFE' TO CSO5-PLAN-CODE

             WHEN CPA-CLAIM-TYPE = 'A'
                  AND CPA-PAYMENT-TYPE NOT = '3'
                  AND DTE-CLIENT = 'DCC'
                 MOVE 'A&H ' TO CSO5-PLAN-CODE

             WHEN CPA-CLAIM-TYPE = 'A'
                  AND CPA-PAYMENT-TYPE = '3'
                  AND DTE-CLIENT = 'DCC'
                 MOVE 'LIFE' TO CSO5-PLAN-CODE

             WHEN CPA-CLAIM-TYPE = 'I'
                 MOVE 'IU  ' TO CSO5-PLAN-CODE

             WHEN CPA-CLAIM-TYPE = 'G'
                 MOVE 'GAP ' TO CSO5-PLAN-CODE
052614
052614       WHEN CPA-CLAIM-TYPE = 'F'
052614           MOVE 'FAM ' TO CSO5-PLAN-CODE
100518
022122       WHEN CPA-CLAIM-TYPE = 'B'
022122           MOVE 'BRV ' TO CSO5-PLAN-CODE
022122
022122       WHEN CPA-CLAIM-TYPE = 'H'
022122           MOVE 'HOS ' TO CSO5-PLAN-CODE
100518
100518       WHEN CPA-CLAIM-TYPE = 'O'
100518           MOVE 'OTH ' TO CSO5-PLAN-CODE

             WHEN OTHER
                 MOVE 'LIFE' TO CSO5-PLAN-CODE
             END-EVALUATE

      *      CLAIM TYPE P WAS ADDED TO FLAG THOSE LIFE CLAIMS WHICH
      *      ARE IN REALITY PROPERTY CLAIMS BEING PROCESSED THRU THE
      *      LOGIC SYSTEM LIFE SECTIONS.  FOR CID CHANGE COMPANY NAME.

             IF CPA-COVERAGE-TYPE = 'P'
                 MOVE 'PROP' TO CSO5-PLAN-CODE
062821           IF DTE-CLIENT EQUAL TO 'CID' OR 'DCC' OR 'AHL' OR 'FNL'
                    MOVE 02-HEADING TO CSO2-COMPANY-NAME
                    MOVE 02-COMP-NAME TO CSO26-COMPANY-NAME
                 END-IF
             END-IF
             
             MOVE CPA-AMOUNT-PAID        TO  CSO5-AMOUNT-PAID
                                             CSO31-CHECK-AMOUNT
             
             MOVE CPA-CHECK-NUMBER       TO  CSO2-CHECK-NUMBER
                                             CSO24-CHECK-NUMBER
             
             IF CPA-CHECK-DATE NOT EQUAL TO LOW-VALUES
                 MOVE CPA-CHECK-DATE     TO  DC-BIN-DATE-1
                 MOVE SPACES             TO  DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                 IF DC-ERROR-CODE EQUAL TO SPACES
                     MOVE DC-GREG-DATE-1-EDIT TO  CSO31-CHECK-DATE
                     MOVE DC-GREG-DATE-A-EDIT TO  WRK-CHECK-DATE
                 END-IF
             END-IF

             MOVE CPA-COMMENT TO WS-CPA-COMMENT

             MOVE CPA-COMMENT              TO  CSO-CHECK-PRINT-LINE-33
             MOVE CPA-COMMENT-2            TO  CSO-CHECK-PRINT-LINE-34

             MOVE CPA-PAYEE-NAME           TO  CSO35-PAYEE-NAME
             MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CSO36-PAYEE-ADDRESS1
             MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CSO37-PAYEE-ADDRESS2
             MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CSO38-PAYEE-ADDRESS3
             MOVE CPA-PAYEE-CITY-STATE     TO  CSO39-PAYEE-ADDRESS4

             IF CPA-PAYEE-ZIP-CODE NOT EQUAL TO ZERO
                 MOVE CPA-PAYEE-ZIP-CODE  TO  CSO40-PAYEE-ZIP-CODE
             END-IF

             IF CSO39-PAYEE-ADDRESS4 EQUAL TO SPACES
                 MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
                 MOVE SPACES               TO CSO40-PAYEE-ADDRESS5
             END-IF

             IF CSO38-PAYEE-ADDRESS3 EQUAL TO SPACES
                 MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
                 MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
                 MOVE SPACES               TO CSO40-PAYEE-ADDRESS5
             END-IF

             IF CSO37-PAYEE-ADDRESS2 EQUAL TO SPACES
                 MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
                 MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
                 MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
                 MOVE SPACES               TO CSO40-PAYEE-ADDRESS5
             END-IF

             IF CSO36-PAYEE-ADDRESS1 EQUAL TO SPACES
                 MOVE CSO37-PAYEE-ADDRESS2 TO CSO36-PAYEE-ADDRESS1
                 MOVE CSO38-PAYEE-ADDRESS3 TO CSO37-PAYEE-ADDRESS2
                 MOVE CSO39-PAYEE-ADDRESS4 TO CSO38-PAYEE-ADDRESS3
                 MOVE CSO40-PAYEE-ADDRESS5 TO CSO39-PAYEE-ADDRESS4
                 MOVE SPACES               TO CSO40-PAYEE-ADDRESS5
             END-IF

             MOVE CPA-NOTIFY-NAME          TO CSO17-3RD-NAME
             MOVE CPA-NOTIFY-ADDRESS-LINE1 TO CSO18-3RDADD-LINE1
             MOVE CPA-NOTIFY-ADDRESS-LINE2 TO CSO19-3RDADD-LINE2
             MOVE CPA-NOTIFY-CITY-STATE    TO CSO20-3RD-CITY-STATE
             MOVE CPA-NOTIFY-ZIP           TO CSO21-3RD-ZIP

091808***CHECK PAYEE ADDRESS FOR ALASKA STATE CODE
091808
091808       MOVE CPA-PAYEE-CITY-STATE TO WS-PAYEE-CITY-STATE
091808       MOVE 'N' TO WS-PAYEE-STATE-FOUND
091808       MOVE 0 TO WS-BEG-SUB WS-END-SUB
091808       PERFORM VARYING WS-SUB FROM 30 BY -1
091808          UNTIL WS-SUB = 0 OR PAYEE-STATE-FOUND
091808         IF WS-END-SUB = 0  AND
091808           (WS-PAYEE-CITY-ST (WS-SUB) EQUAL SPACES OR '.')
091808            CONTINUE
091808         ELSE
091808            IF WS-END-SUB = 0
091808                MOVE WS-SUB TO WS-END-SUB
091808            ELSE
091808                IF WS-PAYEE-CITY-ST (WS-SUB) = ' ' OR ','
091808                    COMPUTE WS-BEG-SUB = WS-SUB + 1 
091808                    MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808                END-IF
091808            END-IF
091808         END-IF
091808       END-PERFORM
091808
091808       IF WS-BEG-SUB > 0
091808          COMPUTE WS-STATE-LENGTH = WS-END-SUB - WS-BEG-SUB + 1
091808          IF WS-STATE-LENGTH = 2
091808             MOVE WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) 
091808                  TO WS-PAYEE-STATE
091808          ELSE
091808             IF WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) =
091808                'ALASKA'
091808                 MOVE 'AK' TO WS-PAYEE-STATE
091808             ELSE
091808                 MOVE 'XX' TO WS-PAYEE-STATE
091808             END-IF
091808          END-IF
091808       ELSE
091808          MOVE 'XX' TO WS-PAYEE-STATE
091808       END-IF

             MOVE LOW-VALUES           TO MICR-RECORD
072110       MOVE LOW-VALUES           TO NAP-LETTER-RECORD       
072110       MOVE X'A2'                TO NLR-TAB00
072110                                    NLR-TAB01
072110                                    NLR-TAB02
072110                                    NLR-TAB03
072110                                    NLR-TAB04
072110                                    NLR-TAB05
072110                                    NLR-TAB06
072110                                    NLR-TAB07
072110                                    NLR-TAB08
072110                                    NLR-TAB09
072110                                    NLR-TAB10
072110                                    NLR-TAB11
072110                                    NLR-TAB12
072110                                    NLR-TAB13
072110                                    NLR-TAB14
072110                                    NLR-TAB15
072110                                    NLR-TAB16
072110                                    NLR-TAB17
072110                                    NLR-TAB18
072110                                    NLR-TAB19
072110                                    NLR-TAB20
072110                                    NLR-TAB21
072110                                    NLR-TAB22
072110                                    NLR-TAB23
072110                                    NLR-TAB24
072110                                    NLR-TAB25
072110                                    NLR-TAB26
072110                                    NLR-TAB27
072110                                    NLR-TAB28
072110                                    NLR-TAB29
072110                                    NLR-TAB30
072110                                    NLR-TAB31
072110                                    NLR-TAB32
072110                                    NLR-TAB33
072110                                    NLR-TAB34
072110                                    NLR-TAB35
072110                                    NLR-TAB36
072110                                    NLR-TAB37
072110                                    NLR-TAB38
072110                                    NLR-TAB39
072110                                    NLR-TAB40
072110                                    NLR-TAB41
072110                                    NLR-TAB42
072110                                    NLR-TAB43
072110                                    NLR-TAB44
072110                                    NLR-TAB45
072110                                    NLR-TAB46
072110                                    NLR-TAB47
072110                                    NLR-TAB48
072110                                    NLR-TAB49
072110                                    NLR-TAB50
072110                                    NLR-TAB51
072110                                    NLR-TAB52
072110                                    NLR-TAB53
072110                                    NLR-TAB54
072110                                    NLR-TAB55
072110                                    NLR-TAB56
072110                                    NLR-TAB57
072110                                    NLR-TAB58
072110                                    NLR-TAB59
072110                                    NLR-TAB60
072110                                    NLR-TAB61
072110                                    NLR-TAB62
072110                                    NLR-TAB63
072110                                    NLR-TAB64
072110                                    NLR-TAB65
072110                                    NLR-TAB66
072110                                    NLR-TAB67
072110                                    NLR-TAB68
072110                                    NLR-TAB69
072110                                    NLR-TAB70
072110                                    NLR-TAB71
072110                                    NLR-TAB72
072110                                    NLR-TAB73
072110                                    NLR-TAB74
072110                                    NLR-TAB75
072110                                    NLR-TAB76
072110                                    NLR-TAB77
072110                                    NLR-TAB78
072110                                    NLR-TAB79
072110                                    NLR-TAB80
072110                                    NLR-TAB81
072110                                    NLR-TAB82
072110                                    NLR-TAB83
072110                                    NLR-TAB84
072110                                    NLR-TAB85
072110                                    NLR-TAB86
072110                                    NLR-TAB87
072110                                    NLR-TAB88
072110                                    NLR-TAB89
072110                                    NLR-TAB90
072110                                    NLR-TAB91
072110                                    NLR-TAB92
092010                                    NLR-TAB93
092010                                    NLR-TAB94
031612                                    NLR-TAB95
072110       MOVE ZEROS                TO NLR-LIFE-INT-RATE
072110       MOVE PARM-CYCLE-DATE      TO NLR-CYCLE-DATE
092010       MOVE SPACES               TO NLR-NEXTBUSDATE
032311       MOVE SPACES               TO WRK-EOB-CODES
032311       MOVE +1                   TO WRK-EOB-SUB
    
071420       evaluate true
071420          when dte-client = 'DCC'
071420             MOVE 'DCCC'         TO M420C-FORM
071420          when dte-client = 'FNL'
071420             move '420F'         TO M420C-FORM
071420          when dte-client = 'AHL'
071420             move '420H'         TO M420C-FORM
071420          when dte-client = 'VPP'
090120             move 'VPPP'         TO M420C-FORM
071420          when other   *> that would be CID
071420             move '420P'         to m420c-form
071420       end-evaluate

111714*       IF DTE-CLIENT = 'AHL'
111714*          IF WS-PAYEE-STATE = 'AK'
111714*              MOVE 'AKAH'  TO M420C-FORM
111714*          ELSE
111714*              MOVE '420A'  TO M420C-FORM
111714*          END-IF
111714*       ELSE             
111714*          IF WS-PAYEE-STATE = 'AK'
111714*            MOVE 'AKCK'    TO M420C-FORM
111714*          ELSE             
111714*            MOVE '420C'    TO M420C-FORM
111714*          END-IF
111714*       END-IF
111714*      END-IF

111714       move zeros                to m420c-draft
111714       move cso2-check-number    to m420c-draft (4:7)

111714*      if dte-client = 'DCC'
111714*         MOVE '0'               TO M420C-DRAFT(1:1)
111714*         MOVE CSO2-CHECK-NUMBER(1:1)
111714*                                TO M420C-DRAFT(2:1)
111714*         MOVE '00'              TO M420C-DRAFT(3:2)
111714*         MOVE CSO2-CHECK-NUMBER(2:6)
111714*                                TO M420C-DRAFT(5:6)
111714*      else
111714*         move zeros             to m420c-draft
111714*         move cso2-check-number to m420c-draft (4:7)
111714*      end-if

072110       MOVE M420C-DRAFT             TO NLR-DRAFT-NO             
             IF WS-DRAFT-ORDER = 99999
                 MOVE ZEROS               TO WS-DRAFT-ORDER
             END-IF

100621       if cpa-ach-payment = 'Y'
100621          move zeros             to m420c-draft-order
100621                                    nlr-draft-order
100621       else
100621          ADD 1                  TO WS-DRAFT-ORDER
100621          MOVE WS-DRAFT-ORDER    TO M420C-DRAFT-ORDER
100621                                    NLR-DRAFT-ORDER
100621       end-if

             MOVE CPA-BENEFICIARY         TO M420C-LOAN-NUMBER
072110                                       NLR-LOAN-NO
             MOVE CPA-AMOUNT-PAID         TO M420C-AMOUNT-PAID
072110                                       NLR-DRAFT-AMOUNT
             MOVE CSO2-COMPANY-NAME       TO M420C-COMPANY-NAME

071420       evaluate true
062821          when dte-client = 'AHL' OR 'FNL'
071420             MOVE 'P.O. BOX 34350   OMAHA, NE  68134'
071420                                 TO M420C-CSO-ADDRESS
071420          when (dte-client = 'DCC')
071420             and (cpa-carrier = '3' or '4')
071420             MOVE 'P.O. BOX 641668  OMAHA, NE  68164'
071420                                 TO M420C-CSO-ADDRESS
071420          when dte-client = 'DCC'
071420             MOVE 'P.O. BOX 642180  OMAHA, NE  68164'
071420                                 TO M420C-CSO-ADDRESS
071420          when dte-client = 'VPP'
071420             MOVE 'P.O. BOX 642180  OMAHA, NE  68164'
071420                                 TO M420C-CSO-ADDRESS
071420          when other  *>  'CID' and anyone else
071420             MOVE 'P.O. BOX 34350   OMAHA, NE  68134'
071420                                 TO M420C-CSO-ADDRESS
071420       end-evaluate

072110       MOVE CPA-CARRIER             TO NLR-CARRIER      
             MOVE CSO5-CLAIM-NO           TO M420C-CLAIM-NO
072110                                       NLR-CLAIM-NO
             MOVE CSO5-CERT-NO            TO M420C-CERT-NO
072110                                       NLR-CERT-NO
             MOVE CSO5-PLAN-CODE          TO M420C-PLAN-CODE
072110                                       NLR-PLAN-CODE
032311
032311       IF WS-KIND = '14E' AND CL-NO-OF-PMTS-MADE = 1
032311          MOVE 'EL14' TO WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311          ADD +1      TO WRK-EOB-SUB
032311       END-IF
032311       IF WS-KIND = '30E' AND CL-NO-OF-PMTS-MADE = 1
032311          MOVE 'EL30' TO WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311          ADD +1      TO WRK-EOB-SUB
032311       END-IF
032311
052614       IF NLR-PLAN-CODE = 'A&H ' OR 'IU  ' OR 'GAP ' OR 'FAM '
022122          OR 'BRV ' OR 'HOS '
061311          IF WS-PAYEE-CD <> 'B'
061311              MOVE 'DFTE'           TO NLR-LETTER-ID
061311          ELSE
061310              MOVE 'DFTA'           TO NLR-LETTER-ID
061311          END-IF
072110          MOVE 'AEOB'               TO NLR-EOB-ID
072110          IF NLR-LOAN-NO = SPACE
072110             MOVE 'AH2 '            TO NLR-COV-LET-ID
072110          ELSE
072110             MOVE 'AH1 '            TO NLR-COV-LET-ID 
072110          END-IF
072110       ELSE
092010          IF CPA-PAYMENT-TYPE = '4' OR
092010            (CPA-PAYMENT-TYPE = '2' AND WS-PAYEE-CD <> 'B')
092010              MOVE 'DFTE'           TO NLR-LETTER-ID
100518          ELSE
100518              IF CPA-CLAIM-TYPE = 'O'
100518                 MOVE 'DFTO'           TO NLR-LETTER-ID
092010              ELSE
092010                 MOVE 'DFTL'           TO NLR-LETTER-ID
100518              END-IF
092010          END-IF
072110          MOVE 'DEOB'               TO NLR-EOB-ID
072110          MOVE 'LIFE'               TO NLR-COV-LET-ID
013013          IF  WRK-PRINT-EOB-IND = 'S'
013013              MOVE 'SEOB'           TO NLR-EOB-ID
013013          END-IF
072110       END-IF
072110       IF NLR-LOAN-NO = SPACE
072110          MOVE 'PLEASE PROVIDE'  TO NLR-EOB-LOAN-NO
072110       ELSE
072110          MOVE NLR-LOAN-NO       TO WRK-LOAN-NUMBER
072110          PERFORM 4100-MASK-LOAN-NO THRU 4100-EXIT
072110          MOVE WRK-LOAN-NUMBER (SUB1:16) TO NLR-EOB-LOAN-NO
072110       END-IF
072110
072110       MOVE CPA-CERT-EFF-DT         TO DC-BIN-DATE-1
072110       MOVE SPACES                  TO DC-OPTION-CODE
072110       PERFORM 8500-DATE-CONVERSION
072110       IF DC-ERROR-CODE EQUAL TO SPACES
072110           MOVE DC-GREG-DATE-A-EDIT TO NLR-CERT-EFF-DT
072110       END-IF
072110       MOVE CPA-INCURRED-DT         TO DC-BIN-DATE-1
072110       MOVE SPACES                  TO DC-OPTION-CODE
072110       PERFORM 8500-DATE-CONVERSION
072110       IF DC-ERROR-CODE EQUAL TO SPACES
072110           MOVE DC-GREG-DATE-A-EDIT TO NLR-CLAIM-INCUR-DT
072110       END-IF
072110
             MOVE CSO5-PAID-FROM-DATE     TO M420C-PAID-FROM-DATE
072110       MOVE WRK-PAID-FROM-DT        TO NLR-PAID-FROM-DT
             MOVE CSO5-PAID-THRU-DATE     TO M420C-PAID-THRU-DATE
072110       MOVE WRK-PAID-THRU-DT        TO NLR-PAID-THRU-DT
072110       MOVE WRK-FORM-DUE-DT         TO NLR-FORM-DUE-DT
             MOVE CSO5-PAYMENT-TYPE       TO M420C-PAYMENT-TYPE
090108       IF WS-AUTO-PAY-SW = 'Y' AND CSO5-PAYMENT-TYPE NOT = 'F'
090108           MOVE 'A'                 TO M420C-PAYMENT-TYPE
090108       END-IF             
072110       MOVE CPA-PAYMENT-TYPE      TO NLR-PMT-TYPE
052614       IF NLR-PLAN-CODE = 'A&H ' OR 'IU  ' OR 'GAP ' OR 'FAM'
022122          OR 'BRV ' OR 'HOS '
072110          IF M420C-PAYMENT-TYPE = 'F'
041813             IF SPECIAL-RELEASE-CHK
041813                 MOVE 'CX'          TO NLR-ENCLOSURE-CD
041813             ELSE
041813                 MOVE 'C'           TO NLR-ENCLOSURE-CD
041813             END-IF
072110             IF DTE-CLIENT = 'CID'
072110                 MOVE 'SRVC'        TO NLR-SURVEY-ID
072110                 MOVE SPACES        TO NLR-DCC-EVENT
072110             ELSE
072110                 IF DTE-CLIENT = 'DCC'
072110                     MOVE 'SRVD'    TO NLR-SURVEY-ID
072110                     IF NLR-PLAN-CODE = 'IU'
072110                         MOVE 'unemployment' TO NLR-DCC-EVENT
072110                     ELSE
072110                         MOVE 'a disability' TO NLR-DCC-EVENT
072110                     END-IF
030612                 ELSE
030612                     MOVE SPACES   TO NLR-SURVEY-ID
030612                     MOVE SPACES   TO NLR-DCC-EVENT
072110                 END-IF
072110             END-IF
072110          ELSE
072110             MOVE SPACES           TO NLR-SURVEY-ID
072110             MOVE SPACES           TO NLR-DCC-EVENT
072110             IF M420C-PAYMENT-TYPE = 'P'
012011               IF NLR-PLAN-CODE = 'IU'
041813                  IF SPECIAL-RELEASE-CHK
041813                      MOVE '7X'    TO NLR-ENCLOSURE-CD
041813                  ELSE
041813                      MOVE '7'     TO NLR-ENCLOSURE-CD
041813                  END-IF
012011               ELSE
041813                  IF SPECIAL-RELEASE-CHK
041813                      MOVE '2X'    TO NLR-ENCLOSURE-CD
041813                  ELSE
041813                      MOVE '2'     TO NLR-ENCLOSURE-CD
041813                  END-IF
012011               END-IF
072110             ELSE
072110                IF CPA-AUTO-PAY-END-DT NOT EQUAL LOW-VALUES 
072110                                                 AND SPACES 
041813                    IF SPECIAL-RELEASE-CHK
041813                        MOVE '0X'  TO NLR-ENCLOSURE-CD
041813                    ELSE
041813                        MOVE '0'   TO NLR-ENCLOSURE-CD
041813                    END-IF
072110                ELSE
012011                 IF NLR-PLAN-CODE = 'IU'
041813                   IF SPECIAL-RELEASE-CHK
041813                      MOVE '7X'    TO NLR-ENCLOSURE-CD
041813                   ELSE
041813                      MOVE '7'     TO NLR-ENCLOSURE-CD
041813                   END-IF
012011                 ELSE
041813                   IF SPECIAL-RELEASE-CHK
041813                      MOVE '2X'    TO NLR-ENCLOSURE-CD
041813                   ELSE
041813                      MOVE '2'     TO NLR-ENCLOSURE-CD
041813                   END-IF
012011                 END-IF
072110                END-IF
072110             END-IF
072110          END-IF
072110       ELSE
072110          MOVE SPACES TO NLR-SURVEY-ID
072110          MOVE SPACES TO NLR-DCC-EVENT
062821          IF DTE-CLIENT = 'AHL' OR 'FNL'
041813              IF SPECIAL-RELEASE-CHK
041813                  MOVE 'EX'    TO NLR-ENCLOSURE-CD
041813              ELSE
041813                  MOVE 'E'     TO NLR-ENCLOSURE-CD
041813              END-IF
030612          ELSE
041813              IF SPECIAL-RELEASE-CHK
041813                  MOVE 'EPX'   TO NLR-ENCLOSURE-CD
041813              ELSE
041813                  MOVE 'EP'    TO NLR-ENCLOSURE-CD
041813              END-IF
030612          END-IF
072110       END-IF
072110
072110       MOVE CPA-NO-OF-PMTS-MADE     TO NLR-NO-OF-PMTS
072110       COMPUTE WRK-PREV-PMTS = CPA-NO-OF-PMTS-MADE - 1
072110       MOVE WRK-PREV-PMTS           TO NLR-PREV-NO-OF-PMTS
072110       MOVE CPA-AMOUNT-PAID-TO-DATE TO NLR-TOTAL-PAID-AMT
072110       COMPUTE WRK-PREV-TOT-AMT = CPA-AMOUNT-PAID-TO-DATE - 
072110                                  CPA-AMOUNT-PAID
072110       MOVE WRK-PREV-TOT-AMT        TO NLR-PREV-TOT-PAID-AMT
072110       MOVE WRK-PREV-PAID-THRU      TO NLR-PREV-PAID-THRU-DT
072110       MOVE WRK-MO-BENEFIT          TO NLR-MONTHLY-BENEFIT
             MOVE CSO5-ACCT-NO            TO M420C-ACCT-NO
072110                                       NLR-ACCT-NO             
             MOVE CSO7-CC-ACCT            TO M420C-CC-ACCT
             MOVE CSO7-CC-ACCT-NUMBER     TO M420C-CC-ACCT-NUMBER
             MOVE CSO7-TYPE-MESSAGE       TO M420C-TYPE-MESSAGE

072110*      IF (CPA-CLAIM-TYPE = 'A')
072110*         AND (DTE-CLIENT = 'CID')
072110*         MOVE 'NOTICE TO INSURED:'
072110*                                TO M420C-FINAL-MESS9
072110*      ELSE
072110*         IF (CPA-CLAIM-TYPE = 'A' OR 'I' OR 'G')
072110*            AND (DTE-CLIENT = 'DCC')
072110*            IF  CM-ACCOUNT (9:2) = 'BI'
072110*                MOVE 'NOTE TO BUYER:' TO M420C-FINAL-MESS9
072110*            ELSE
072110*                MOVE 'NOTE TO BORROWER:'
072110*                                TO M420C-FINAL-MESS9
072110*            END-IF                                       
072110*         ELSE
                   MOVE SPACES         TO M420C-FINAL-MESS9
072110*         END-IF
072110*      END-IF

             MOVE SPACES                  TO M420C-FINAL-MESS10
             MOVE SPACES                  TO M420C-FINAL-MESS11
             MOVE CSO11-MEMBER-NAME       TO M420C-MEMBER-NAME
072110       MOVE CL-INSURED-LAST-NAME    TO NLR-INS-LAST-NAME
072110       MOVE CL-INSURED-1ST-NAME     TO NLR-INS-FIRST-NAME
072110       MOVE CL-INSURED-SEX-CD       TO NLR-INS-SEX-CODE
031612       MOVE CL-CCN (1:9)            TO NLR-AHL-CLAIM-NO

             MOVE CPA-INSURED-ADDRESS-LINE1 TO M420C-MEMBER-ADDRESS1
             MOVE CPA-INSURED-ADDRESS-LINE2 TO M420C-MEMBER-ADDRESS2
             MOVE CPA-INSURED-ADDRESS-LINE3 TO M420C-MEMBER-ADDRESS3
072110       IF CPA-INSURED-ADDRESS-LINE1 EQUAL SPACES
072110           MOVE CPA-INSURED-ADDRESS-LINE2 TO NLR-INS-ADDR1
072110           MOVE CPA-INSURED-ADDRESS-LINE3 TO NLR-INS-ADDR2
072110       ELSE
072110           MOVE CPA-INSURED-ADDRESS-LINE1 TO NLR-INS-ADDR1
072110           MOVE CPA-INSURED-ADDRESS-LINE2 TO NLR-INS-ADDR2
072110       END-IF             
072110*             MOVE CPA-INSURED-CITY-STATE    TO M420C-MEMBER-ADDRESS4
072110       MOVE SPACES                    TO M420C-MEMBER-ADDRESS4
072110       STRING CPA-INSURED-CITY ' ' CPA-INSURED-STATE
072110          DELIMITED BY '  ' INTO M420C-MEMBER-ADDRESS4
072110       END-STRING
072110       MOVE CPA-INSURED-CITY          TO NLR-INS-CITY
072110       MOVE CPA-INSURED-STATE         TO NLR-INS-STATE
             MOVE CPA-INSURED-ZIP-CODE      TO CSO-ZIP
             MOVE CSO-ZIP                   TO M420C-MEMBER-ZIP-CODE
072110                                         NLR-INS-ZIP
             MOVE CSO17-3RD-NAME          TO M420C-3RDADD-NAME
             MOVE CSO18-3RDADD-LINE1      TO M420C-3RDADD-LINE1
             MOVE CSO19-3RDADD-LINE2      TO M420C-3RDADD-LINE2
             MOVE CSO20-3RD-CITY-STATE    TO M420C-3RDADD-LINE3
             MOVE CSO21-3RD-ZIP           TO M420C-3RDADD-ZIP
             MOVE CSO31-CHECK-DATE        TO M420C-CHECK-DATE
072110       MOVE WRK-CHECK-DATE          TO NLR-DRAFT-DT
072110****DRAFT NOTES EXPANDED TO 60.  SINCE MESSAGES ARE NO LONGER USED
072110****DRAFT NOTES WILL BE PUT INTO THE FIRST TWO MESSAGE FIELDS
072110*       MOVE CSO-CHECK-PRINT-LINE-33 TO M420C-DFT-NOTES1
072110*       MOVE CSO-CHECK-PRINT-LINE-34 TO M420C-DFT-NOTES2
072110       MOVE SPACES                  TO M420C-DFT-NOTES1
072110                                       M420C-DFT-NOTES2
072110       MOVE CPA-COMMENT             TO NLR-DRAFT-NOTE-1
072110       MOVE CPA-COMMENT-2           TO WRK-DRAFT-NOTE-2
072110       IF WRK-NOTE-CODE-IND = 'Y'
072110           MOVE SPACES              TO NLR-DRAFT-NOTE-2
072110           MOVE WRK-NOTE-CODE-1     TO NLR-DRAFT-NOTE-3
072110           MOVE WRK-NOTE-CODE-2     TO NLR-DRAFT-NOTE-4
072110           MOVE WRK-NOTE-CODE-3     TO NLR-DRAFT-NOTE-5
072110           MOVE WRK-NOTE-CODE-4     TO NLR-DRAFT-NOTE-6
072110           MOVE WRK-NOTE-CODE-5     TO NLR-DRAFT-NOTE-7
072110           MOVE WRK-NOTE-CODE-6     TO NLR-DRAFT-NOTE-8
072110           MOVE WRK-NOTE-CODE-7     TO NLR-DRAFT-NOTE-9
072110           MOVE WRK-NOTE-CODE-8     TO NLR-DRAFT-NOTE-10
072110           MOVE WRK-NOTE-CODE-9     TO NLR-DRAFT-NOTE-11
072110           MOVE WRK-NOTE-CODE-10    TO NLR-DRAFT-NOTE-12
072110           MOVE WRK-NOTE-CODE-11    TO NLR-DRAFT-NOTE-13
072110           MOVE WRK-NOTE-CODE-12    TO NLR-DRAFT-NOTE-14
072110       ELSE
072110           MOVE WRK-DRAFT-NOTE-2    TO NLR-DRAFT-NOTE-2
072110           MOVE SPACES              TO NLR-DRAFT-NOTE-3
072110                                       NLR-DRAFT-NOTE-4
072110                                       NLR-DRAFT-NOTE-5
072110                                       NLR-DRAFT-NOTE-6
072110                                       NLR-DRAFT-NOTE-7
072110                                       NLR-DRAFT-NOTE-8
072110                                       NLR-DRAFT-NOTE-9
072110                                       NLR-DRAFT-NOTE-10
072110                                       NLR-DRAFT-NOTE-11
072110                                       NLR-DRAFT-NOTE-12
072110                                       NLR-DRAFT-NOTE-13
072110                                       NLR-DRAFT-NOTE-14
072110       END-IF
092010       IF WS-AUTO-PAY-SW = 'Y' AND CSO5-PAYMENT-TYPE NOT = 'F'
092010          AND (CPA-AUTO-PAY-END-DT EQUAL LOW-VALUES OR SPACES) 
092010             MOVE 'AUTO'            TO NLR-DRAFT-NOTE-3
092010       END-IF
013017       MOVE CPA-ACH-PAYMENT      TO M420C-ACH-PAYMENT
             MOVE CSO35-PAYEE-NAME        TO M420C-PAYEE-NAME
072110                                       NLR-PAYEE-NAME
             MOVE CPA-PAYEE-ADDRESS-LINE1 TO M420C-PAYEE-ADDRESS1
072110                                       NLR-PAYEE-ADDR1
             MOVE CPA-PAYEE-ADDRESS-LINE2 TO M420C-PAYEE-ADDRESS2
072110                                       NLR-PAYEE-ADDR2
             MOVE CPA-PAYEE-ADDRESS-LINE3 TO M420C-PAYEE-ADDRESS3
072110*             MOVE CPA-PAYEE-CITY-STATE    TO M420C-PAYEE-ADDRESS4
072110       MOVE SPACES                  TO M420C-PAYEE-ADDRESS4
072110       STRING CPA-PAYEE-CITY ' ' CPA-PAYEE-STATE
072110          DELIMITED BY '  ' INTO M420C-PAYEE-ADDRESS4
072110       END-STRING
092010       IF CPA-PAYEE-ADDRESS-LINE1 EQUAL SPACES
092010          MOVE CPA-PAYEE-ADDRESS-LINE2 TO NLR-PAYEE-ADDR1
092010          MOVE CPA-PAYEE-ADDRESS-LINE3 TO NLR-PAYEE-ADDR2
092010       END-IF
072110       MOVE CPA-PAYEE-CITY          TO NLR-PAYEE-CITY
072110       MOVE CPA-PAYEE-STATE         TO NLR-PAYEE-STATE
             MOVE CPA-PAYEE-ZIP-CODE      TO CSO-ZIP
             MOVE CSO-ZIP                 TO M420C-PAYEE-ZIP-CODE
072110                                       NLR-PAYEE-ZIP
092010       MOVE WS-PAYEE-CODE           TO NLR-PAYEE-CODE
072110       MOVE CPA-ACCOUNT-NAME        TO NLR-ACCT-NAME
072110       MOVE CPA-ACCOUNT-ADDRESS-LINE1 TO NLR-ACCT-ADDR1
072110       MOVE CPA-ACCOUNT-ADDRESS-LINE2 TO NLR-ACCT-ADDR2
092010       IF CPA-ACCOUNT-CITY (1:10) EQUAL 'REFUND DIR'
092010           MOVE SPACES              TO NLR-ACCT-CITY
092010                                       NLR-ACCT-STATE
092010       ELSE
092010           MOVE CPA-ACCOUNT-CITY    TO NLR-ACCT-CITY
092010           MOVE CPA-ACCOUNT-STATE   TO NLR-ACCT-STATE
092010       END-IF
072110       MOVE CPA-ACCOUNT-ZIP-CODE    TO CSO-ZIP
072110       MOVE CSO-ZIP                 TO NLR-ACCT-ZIP
072110
072110       PERFORM 4050-GET-BENE-NAME-ADDR THRU 4050-EXIT
072110
             MOVE CSO23-REPLY-DT          TO M420C-REPLY-DATE
      ****   ***
      * US    A IF YOU WANT BILL KIZER, AUTHORIZED SIGNATURE OR
      * B    F YOU WANT BILL KIZER, PRESIDENT
      ****   ***
             IF M420C-AMOUNT-PAID > 100000.00
               MOVE 'C'                   TO M420C-SIGNATURE
             ELSE
               MOVE 'B'                   TO M420C-SIGNATURE
             END-IF

             MOVE SPACE TO M420C-DRAFT-MESSAGES
013013       IF WRK-PRINT-EOB-IND <> 'Y' AND 'S'
072110           MOVE CPA-COMMENT         TO M420C-DRAFT-MESSAGE(1)
072110           MOVE CPA-COMMENT-2       TO M420C-DRAFT-MESSAGE(2)
072110           MOVE 'DRAFT COPY'        TO M420C-FINAL-MESS11
072110       END-IF


             MOVE CPA-STATE               TO M420C-ACCT-STATE
072110                                       NLR-STATE-CODE
             MOVE CPA-CLAIM-TYPE          TO M420C-CLAIM-TYPE
072110                                       NLR-CLAIM-TYPE

             DISPLAY 'CONTROL NUMBER: ' CQ-CONTROL-NUMBER-A1
                    ' CHECK NUMBER: ' M420C-DRAFT

             WRITE MICR-RECORD
             IF MICR-STATUS = '00'
                ADD +1 TO DRAFTS-WRITTEN
             ELSE
                MOVE MICR-STATUS TO WS-ABEND-FILE-STATUS
                MOVE 'WRITE ERROR ON MICRDRFT FILE' TO WS-ABEND-MESSAGE
                PERFORM ABEND-PGM
             END-IF

013017       if cpa-ach-payment = 'Y'
013017          perform 6000-load-ach-table thru 6000-exit
013017       end-if

030311       MOVE DTE-CLASIC-COMPANY-CD   TO WRK-COMPANY
030311       MOVE WRK-COMPANY             TO BC-COMPANY
030311       MOVE NLR-CARRIER             TO BC-CARRIER
072110       MOVE NLR-CERT-NO             TO BC-CERT-NO
072110       MOVE NLR-CLAIM-NO            TO BC-CLAIM-NO
072110       MOVE WS-DRAFT-ORDER          TO BC-SEQ
072110       MOVE CID-BARCODE             TO NLR-BARCODE
072110       MOVE WRK-CERT-EXP-DT         TO NLR-CERT-EXP-DT
072110       MOVE WS-AUTO-PAY-SW          TO NLR-AUTO-PAY
072110       MOVE WRK-AUTO-PAY-END-DT     TO NLR-AUTO-PAY-END-DT
072110
082218       MOVE LOW-VALUES TO WS-HOLD-UNTIL-DT
082218
072110       PERFORM 4200-TOTAL-PAYMENTS THRU 4200-EXIT
072110       MOVE WRK-PAYEE-NAME-1        TO NLR-PAYEE-NAME-1
072110       MOVE WRK-TOTAL-AMT-PAID-1    TO NLR-TOTAL-AMT-PAID-1
072110       MOVE WRK-PAID-FROM-1         TO NLR-PAID-FROM-1
072110       MOVE WRK-PAID-THRU-1         TO NLR-PAID-THRU-1
072110       MOVE WRK-PAYEE-NAME-2        TO NLR-PAYEE-NAME-2
072110       MOVE WRK-TOTAL-AMT-PAID-2    TO NLR-TOTAL-AMT-PAID-2
072110       MOVE WRK-PAID-FROM-2         TO NLR-PAID-FROM-2
072110       MOVE WRK-PAID-THRU-2         TO NLR-PAID-THRU-2
072110       MOVE WRK-PAYEE-NAME-3        TO NLR-PAYEE-NAME-3
072110       MOVE WRK-TOTAL-AMT-PAID-3    TO NLR-TOTAL-AMT-PAID-3
072110       MOVE WRK-PAID-FROM-3         TO NLR-PAID-FROM-3
072110       MOVE WRK-PAID-THRU-3         TO NLR-PAID-THRU-3
072110       MOVE WRK-PAYEE-NAME-4        TO NLR-PAYEE-NAME-4
072110       MOVE WRK-TOTAL-AMT-PAID-4    TO NLR-TOTAL-AMT-PAID-4
072110       MOVE WRK-PAID-FROM-4         TO NLR-PAID-FROM-4
072110       MOVE WRK-PAID-THRU-4         TO NLR-PAID-THRU-4
092010
082218       IF WS-HOLD-UNTIL-DT > LOW-VALUES
082218          MOVE WS-HOLD-UNTIL-DT TO DC-BIN-DATE-1
082218          MOVE +0               TO DC-ELAPSED-MONTHS
082218          MOVE +30              TO DC-ELAPSED-DAYS
082218          MOVE '6'              TO DC-OPTION-CODE
082218          PERFORM 8500-DATE-CONVERSION
082218          IF DC-ERROR-CODE EQUAL TO SPACES
082218             MOVE DC-GREG-DATE-A-EDIT TO NLR-FORM-DUE-DT
082218          END-IF
082218       END-IF
082218
032311       PERFORM VARYING WRK-EOB-SUB FROM 1 BY 1
032311         UNTIL WRK-EOB-SUB > 12
032311           IF WRK-EOB-LU-CODE (WRK-EOB-SUB) > SPACES
032311             EVALUATE TRUE
032311              WHEN NLR-DRAFT-NOTE-3 = SPACES OR
032311               NLR-DRAFT-NOTE-3 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-3
032311              WHEN NLR-DRAFT-NOTE-4 = SPACES OR
032311               NLR-DRAFT-NOTE-4 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-4
032311              WHEN NLR-DRAFT-NOTE-5 = SPACES OR
032311               NLR-DRAFT-NOTE-5 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-5
032311              WHEN NLR-DRAFT-NOTE-6 = SPACES OR
032311               NLR-DRAFT-NOTE-6 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-6
032311              WHEN NLR-DRAFT-NOTE-7 = SPACES OR
032311               NLR-DRAFT-NOTE-7 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-7
032311              WHEN NLR-DRAFT-NOTE-8 = SPACES OR
032311               NLR-DRAFT-NOTE-8 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-8
032311              WHEN NLR-DRAFT-NOTE-9 = SPACES OR
032311               NLR-DRAFT-NOTE-9 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-9
032311              WHEN NLR-DRAFT-NOTE-10 = SPACES OR
032311               NLR-DRAFT-NOTE-10 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-10
032311              WHEN NLR-DRAFT-NOTE-11 = SPACES OR
032311               NLR-DRAFT-NOTE-11 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-11
032311              WHEN NLR-DRAFT-NOTE-12 = SPACES OR
032311               NLR-DRAFT-NOTE-12 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-12
032311              WHEN NLR-DRAFT-NOTE-13 = SPACES OR
032311               NLR-DRAFT-NOTE-13 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-13
032311              WHEN NLR-DRAFT-NOTE-14 = SPACES OR
032311               NLR-DRAFT-NOTE-14 = WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311                  MOVE WRK-EOB-LU-CODE (WRK-EOB-SUB) TO 
032311                      NLR-DRAFT-NOTE-14
032311             END-EVALUATE
032311           END-IF
032311       END-PERFORM
072110
072110       MOVE 'E'                     TO NLR-LAST-BYTE
072110
072110       INSPECT NAP-LETTER-RECORD REPLACING ALL '|' BY ' '
072110       INSPECT NAP-LETTER-RECORD REPLACING ALL X'A2' BY '|'
072110
013013       IF WRK-PRINT-EOB-IND <> 'Y' AND 'S'
072110           MOVE SPACES            TO NLR-EOB-ID
072110       END-IF
020413       IF WRK-PRINT-SURVEY = 'N'
020413           MOVE SPACES            TO NLR-SURVEY-ID
020413       END-IF
020413       IF WRK-PRINT-CLM-FORM = 'N'
041813           IF SPECIAL-RELEASE-CHK
041813               MOVE '0X'          TO NLR-ENCLOSURE-CD
041813           ELSE
041813               MOVE '0'           TO NLR-ENCLOSURE-CD
041813           END-IF
020413       END-IF
072110       WRITE NAPERSOFT-LETTER FROM NAP-LETTER-RECORD

           END-PERFORM.

       0340-MAIN-LOGIC.
           MOVE SPACES                 TO  ACCOUNT-MASTER.

           MOVE ZERO                   TO  AM-ZIP
                                           AM-TEL-NO
                                           AM-TOL-PREM
                                           AM-TOL-REF
                                           AM-TOL-CLM.

           MOVE 'ACCOUNT NOT FOUND'    TO  AM-NAME.

       0360-MAIN-LOGIC.

           IF AT-SEQUENCE-NO = +0
              PERFORM 0360-NO-TRLR
           ELSE
              READ ELTRLR KEY IS AT-CONTROL-PRIMARY
              EVALUATE TRLR-STATUS
                WHEN '00'
                  CONTINUE
                WHEN '23'
                  PERFORM 0360-NO-TRLR
                WHEN OTHER
                  MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
                  MOVE 'READ ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
                  PERFORM ABEND-PGM
              END-EVALUATE
              MOVE AT-MAIL-TO-NAME    TO  CPA-PAYEE-NAME
              MOVE AT-ADDRESS-LINE-1  TO  CPA-PAYEE-ADDRESS-LINE2
              MOVE AT-ADDRESS-LINE-2  TO  CPA-PAYEE-ADDRESS-LINE3
051810        MOVE SPACES             TO  CPA-PAYEE-CITY-STATE
072110*051810        STRING AT-CITY ' ' AT-STATE
072110*051810           DELIMITED BY '  ' INTO CPA-PAYEE-CITY-STATE
072110*051810        END-STRING
072110        MOVE AT-CITY            TO  CPA-PAYEE-CITY
072110        MOVE AT-STATE           TO  CPA-PAYEE-STATE
              MOVE AT-ZIP             TO  CPA-PAYEE-ZIP
           END-IF.

       0360-NO-TRLR.
           MOVE WS-NAME-WORK           TO  CPA-PAYEE-NAME.
           MOVE ZERO                   TO  CPA-PAYEE-ZIP.
           MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE2
                                           CPA-PAYEE-ADDRESS-LINE3
                                           CPA-PAYEE-CITY-STATE.
           MOVE 'Y' TO WS-CHECKS-WITHOUT-ADDRESSES.

       1000-GET-CARRIER-NAME.

           MOVE DTE-CLIENT             TO  CF-CONTROL-PRIMARY.
           MOVE '6'                    TO  CF-RECORD-TYPE.
           MOVE CQ-CARRIER             TO  CF-CARRIER-CNTL.
           MOVE ZERO                   TO  CF-SEQUENCE-NO.

           READ ELCNTL KEY IS CF-CONTROL-PRIMARY
           EVALUATE CNTL-STATUS
             WHEN '00'
               CONTINUE
             WHEN '23'
               MOVE SPACES             TO  WS-CARRIER-ADDRESS-DATA
               EXIT PARAGRAPH
             WHEN OTHER
               MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-EVALUATE

           MOVE CF-MAIL-TO-NAME        TO  WS-CARRIER-MAIL-TO-NAME.
           MOVE CF-IN-CARE-OF          TO  WS-CARRIER-IN-CARE-OF.
           MOVE CF-ADDRESS-LINE-1      TO  WS-CARRIER-ADDRESS-LINE-1.
           MOVE CF-ADDRESS-LINE-2      TO  WS-CARRIER-ADDRESS-LINE-2.
           MOVE CF-CITY-STATE          TO  WS-CARRIER-CITY-STATE.

           IF CF-ZIP-CODE-NUM NOT NUMERIC
               MOVE ZEROS              TO CF-ZIP-CODE-NUM
           END-IF

           IF CF-ZIP-CODE-NUM NOT = ZEROS
               MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-UNPACKED
               MOVE WS-ZIP-UNPACKED    TO  WS-CARRIER-ZIP-CODE
           ELSE
               MOVE CF-ZIP-CODE        TO  WS-CARRIER-ZIP-CODE
           END-IF

           MOVE CF-PHONE-NO            TO  WS-CARRIER-PHONE-NO.

       2000-GET-BENEFICIARY.

           MOVE DTE-CLASIC-COMPANY-CD  TO  BE-COMPANY-CD.
           MOVE 'B'                    TO  BE-RECORD-TYPE.

           READ ELBENE KEY IS BE-CONTROL-PRIMARY
           EVALUATE BENE-STATUS
             WHEN '00'
               CONTINUE
             WHEN '23'
               MOVE SPACES                 TO  CPA-PAYEE-NAME
                                               CPA-PAYEE-ADDRESS-LINE1
                                               CPA-PAYEE-ADDRESS-LINE2
                                               CPA-PAYEE-CITY-STATE
               MOVE ZERO                   TO  CPA-PAYEE-ZIP
               EXIT PARAGRAPH
             WHEN OTHER
               MOVE BENE-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ ERROR ON ELBENE FILE' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-EVALUATE

           MOVE BE-MAIL-TO-NAME        TO  CPA-PAYEE-NAME.
           MOVE BE-ADDRESS-LINE-1      TO  CPA-PAYEE-ADDRESS-LINE1.
           MOVE BE-ADDRESS-LINE-2      TO  CPA-PAYEE-ADDRESS-LINE2.
           MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE3.
051810     MOVE SPACES                 TO  CPA-PAYEE-CITY-STATE.
072110*051810     STRING BE-CITY ' ' BE-STATE
072110*051810        DELIMITED BY '  ' INTO CPA-PAYEE-CITY-STATE
072110*051810     END-STRING
072110     MOVE BE-CITY                TO  CPA-PAYEE-CITY.
072110     MOVE BE-STATE               TO  CPA-PAYEE-STATE.
           MOVE BE-ZIP-CODE            TO  CPA-PAYEE-ZIP.

           .
072110 4050-GET-BENE-NAME-ADDR SECTION.
072110
072110     MOVE SPACES                 TO NLR-BENE-NAME
072110     MOVE SPACES                 TO NLR-BENE-ADDR1
072110     MOVE SPACES                 TO NLR-BENE-ADDR2
072110     MOVE SPACES                 TO NLR-BENE-CITY
072110     MOVE SPACES                 TO NLR-BENE-STATE
072110     MOVE SPACES                 TO NLR-BENE-ZIP
072110     MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
072110     MOVE +11                    TO AT-SEQUENCE-NO
072110
072110     READ ELTRLR
072110
072110     IF (TRLR-STATUS = '00')
072110        AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
072110        IF AT-SEQUENCE-NO = +11
072110           MOVE AT-MAIL-TO-NAME  TO NLR-BENE-NAME
072110           MOVE AT-ADDRESS-LINE-1
072110                                 TO NLR-BENE-ADDR1
072110           MOVE AT-ADDRESS-LINE-2
072110                                 TO NLR-BENE-ADDR2
072110           MOVE AT-CITY          TO NLR-BENE-CITY
072110           MOVE AT-STATE         TO NLR-BENE-STATE
072110           MOVE AT-ZIP           TO NLR-BENE-ZIP
072110        END-IF
072110     END-IF.
072110
072110     IF NLR-BENE-NAME NOT EQUAL SPACES
072110        GO TO 4050-EXIT
072110     END-IF.
072110
072110     MOVE CL-BENEFICIARY         TO  BE-BENEFICIARY.
072110     MOVE DTE-CLASIC-COMPANY-CD  TO  BE-COMPANY-CD.
072110     MOVE 'B'                    TO  BE-RECORD-TYPE.
072110
072110     READ ELBENE KEY IS BE-CONTROL-PRIMARY
072110     EVALUATE BENE-STATUS
072110       WHEN '00'
072110         CONTINUE
072110       WHEN '23'
072110         GO TO 4050-EXIT
072110       WHEN OTHER
072110         MOVE BENE-STATUS TO WS-ABEND-FILE-STATUS
072110         MOVE 'READ ERROR ON ELBENE FILE' TO WS-ABEND-MESSAGE
072110         PERFORM ABEND-PGM
072110     END-EVALUATE
072110
072110
072110     MOVE BE-MAIL-TO-NAME        TO  NLR-BENE-NAME.
072110     MOVE BE-ADDRESS-LINE-1      TO  NLR-BENE-ADDR1.
072110     MOVE BE-ADDRESS-LINE-2      TO  NLR-BENE-ADDR2.
072110     MOVE BE-CITY                TO  NLR-BENE-CITY.
072110     MOVE BE-STATE               TO  NLR-BENE-STATE
072110     MOVE BE-ZIP-CODE            TO  NLR-BENE-ZIP.
072110
072110     .
072110 4050-EXIT.
072110     EXIT.

072110 4100-MASK-LOAN-NO SECTION.
072110       
072110      MOVE 'N'                 TO WRK-END-FOUND.
072110      MOVE +0                  TO SUB1 SUB2
072110      PERFORM VARYING SUB FROM 25 BY -1
072110              UNTIL END-FOUND OR SUB = 0
072110          IF WRK-LOAN-NO-CHAR (SUB) NOT EQUAL SPACE
072110              MOVE 'Y'         TO WRK-END-FOUND
072110              IF SUB > 15
072110                 COMPUTE SUB1 = SUB - 15
072110              ELSE
072110                 MOVE +1      TO SUB1
072110              END-IF
072110              COMPUTE SUB2 = SUB - 4
072110          END-IF
072110      END-PERFORM.
072110
072110      IF SUB2 <= 0 
072110          GO TO 4100-EXIT
072110      END-IF.
072110
072110      INSPECT WRK-LOAN-NUMBER (1:SUB2) CONVERTING 
072110          'ABCDEFGHIJKLMNOPQRSTUVWYZ' TO 
072110          'XXXXXXXXXXXXXXXXXXXXXXXXX'.
072110      INSPECT WRK-LOAN-NUMBER (1:SUB2) CONVERTING
072110          '0123456789' TO 'XXXXXXXXXX'.
072110
072110 4100-EXIT.
072110     EXIT.
072110
072110 4200-TOTAL-PAYMENTS.
072110
072110       MOVE SPACES                 TO WRK-PAYEE-NAME-1
072110                                      WRK-PAYEE-NAME-2
072110                                      WRK-PAYEE-NAME-3
072110                                      WRK-PAYEE-NAME-4.
072110       MOVE +0                     TO WRK-TOTAL-AMT-PAID-1
072110                                      WRK-TOTAL-AMT-PAID-2
072110                                      WRK-TOTAL-AMT-PAID-3
072110                                      WRK-TOTAL-AMT-PAID-4.
072110       MOVE LOW-VALUES             TO WRK-SAVE-THRU-1
072110                                      WRK-SAVE-THRU-2
072110                                      WRK-SAVE-THRU-3
072110                                      WRK-SAVE-THRU-4.
072110       MOVE HIGH-VALUES            TO WRK-SAVE-FROM-1
072110                                      WRK-SAVE-FROM-2
072110                                      WRK-SAVE-FROM-3
072110                                      WRK-SAVE-FROM-4.
072110       MOVE SPACES                 TO WRK-PAID-FROM-1
072110                                      WRK-PAID-THRU-1
072110                                      WRK-PAID-FROM-2
072110                                      WRK-PAID-THRU-2
072110                                      WRK-PAID-FROM-3
072110                                      WRK-PAID-THRU-3
072110                                      WRK-PAID-FROM-4
072110                                      WRK-PAID-THRU-4.
072110
072110       MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.
072110       MOVE +100   TO  AT-SEQUENCE-NO.
072110
072110       START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY.
072110       IF TRLR-STATUS = '00'
072110          CONTINUE
072110       ELSE
072110          MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
072110          MOVE 'START ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
072110          PERFORM ABEND-PGM
072110       END-IF.
072110       
072110 4200-TRLR-READ-LOOP.
072110 
072110       READ ELTRLR NEXT RECORD.
072110       IF TRLR-STATUS = '00'
072110          CONTINUE
072110       ELSE
072110         IF TRLR-STATUS = '10'
072110            GO TO 4200-EXIT
072110         ELSE
072110            MOVE TRLR-STATUS TO WS-ABEND-FILE-STATUS
072110            MOVE 'READ ERROR ON ELTRLR FILE' TO WS-ABEND-MESSAGE
072110            PERFORM ABEND-PGM
072110         END-IF
072110       END-IF.
072110       
072110       IF CL-CONTROL-PRIMARY NOT EQUAL AT-CONTROL-PRIMARY (1:20)
072110           GO TO 4200-EXIT
072110       END-IF.
092010
092010       IF CORRESPONDENCE-TR AND
092010           AT-STD-LETTER-FORM = 'CI05' AND
092010          (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES)
032311              MOVE 'CI05' TO WRK-EOB-LU-CODE (WRK-EOB-SUB)
032311              ADD +1 TO WRK-EOB-SUB
092010       END-IF
072110
072110       IF NOT PAYMENT-TR
072110           GO TO 4200-TRLR-READ-LOOP
072110       END-IF.
072110       
072110       IF AT-VOID-DT NOT EQUAL LOW-VALUES
072110          GO TO 4200-TRLR-READ-LOOP
072110       END-IF.
082218
082218       IF AT-TO-BE-WRITTEN-DT > LOW-VALUES
082218         AND AT-CHECK-WRITTEN-DT = LOW-VALUES
082218         AND AT-VOID-DT = LOW-VALUES
082218         AND AT-PAID-THRU-DT > WS-HOLD-UNTIL-DT
082218          MOVE AT-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
082218       END-IF
092010
062711       IF AT-PAID-THRU-DT GREATER THAN CPA-PAID-THRU-DT
062711          GO TO 4200-TRLR-READ-LOOP
062711       END-IF.
062711
092010       IF AT-AMOUNT-PAID NOT EQUAL ZERO AND
092010         (AT-PAYEES-NAME EQUAL SPACES OR LOW-VALUES)
092010            MOVE 'Adjustment to Claim' TO AT-PAYEES-NAME
092010       END-IF
072110       
030612*****SUMMARIZE AHL PAYMENTS PRIOR TO 4/1/2012 TOGETHER
030612       IF DTE-CLIENT = 'AHL'
030612          IF AT-CHECK-WRITTEN-DT < X'A861'
030612              MOVE 'AHL SUMMARY' TO AT-PAYEES-NAME
030612          END-IF
030612       END-IF
030612
072110       IF WRK-PAYEE-NAME-1 EQUAL SPACES OR
072110          WRK-PAYEE-NAME-1 EQUAL AT-PAYEES-NAME
072110           MOVE AT-PAYEES-NAME  TO  WRK-PAYEE-NAME-1
072110           ADD  AT-AMOUNT-PAID  TO  WRK-TOTAL-AMT-PAID-1
072110           IF AT-PAID-FROM-DT LESS THAN WRK-SAVE-FROM-1
072110              MOVE AT-PAID-FROM-DT   TO  WRK-SAVE-FROM-1
072110              MOVE AT-PAID-FROM-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-FROM-1
072110              END-IF
072110           END-IF
072110           IF AT-PAID-THRU-DT GREATER THAN WRK-SAVE-THRU-1
072110              MOVE AT-PAID-THRU-DT   TO  WRK-SAVE-THRU-1
072110              MOVE AT-PAID-THRU-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-THRU-1
072110              END-IF
072110           END-IF
072110           GO TO 4200-TRLR-READ-LOOP
072110       END-IF.
072110       
072110       IF WRK-PAYEE-NAME-2 EQUAL SPACES OR 
072110          WRK-PAYEE-NAME-2 EQUAL AT-PAYEES-NAME 
072110           MOVE AT-PAYEES-NAME  TO  WRK-PAYEE-NAME-2
072110           ADD  AT-AMOUNT-PAID  TO  WRK-TOTAL-AMT-PAID-2
072110           IF AT-PAID-FROM-DT LESS THAN WRK-SAVE-FROM-2
072110              MOVE AT-PAID-FROM-DT   TO  WRK-SAVE-FROM-2
072110              MOVE AT-PAID-FROM-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-FROM-2
072110              END-IF
072110           END-IF
072110           IF AT-PAID-THRU-DT GREATER THAN WRK-SAVE-THRU-2
072110              MOVE AT-PAID-THRU-DT   TO  WRK-SAVE-THRU-2
072110              MOVE AT-PAID-THRU-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-THRU-2
072110              END-IF
072110           END-IF
072110           GO TO 4200-TRLR-READ-LOOP
072110       END-IF.
072110       
072110       IF WRK-PAYEE-NAME-3 EQUAL SPACES OR 
072110          WRK-PAYEE-NAME-3 EQUAL AT-PAYEES-NAME 
072110           MOVE AT-PAYEES-NAME  TO  WRK-PAYEE-NAME-3
072110           ADD  AT-AMOUNT-PAID  TO  WRK-TOTAL-AMT-PAID-3
072110           IF AT-PAID-FROM-DT LESS THAN WRK-SAVE-FROM-3
072110              MOVE AT-PAID-FROM-DT   TO  WRK-SAVE-FROM-3
072110              MOVE AT-PAID-FROM-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-FROM-3
072110              END-IF
072110           END-IF
072110           IF AT-PAID-THRU-DT GREATER THAN WRK-SAVE-THRU-3
072110              MOVE AT-PAID-THRU-DT   TO  WRK-SAVE-THRU-3
072110              MOVE AT-PAID-THRU-DT   TO  DC-BIN-DATE-1
072110              MOVE SPACES            TO  DC-OPTION-CODE
072110              PERFORM 8500-DATE-CONVERSION
072110              IF DC-ERROR-CODE EQUAL TO SPACES
072110                 MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-THRU-3
072110              END-IF
072110           END-IF
072110           GO TO 4200-TRLR-READ-LOOP
072110       END-IF.
072110       
072110       MOVE 'OTHERS PAID'       TO WRK-PAYEE-NAME-4.
072110       ADD  AT-AMOUNT-PAID      TO WRK-TOTAL-AMT-PAID-4.
072110       IF AT-PAID-FROM-DT LESS THAN WRK-SAVE-FROM-4
072110          MOVE AT-PAID-FROM-DT   TO  WRK-SAVE-FROM-4
072110          MOVE AT-PAID-FROM-DT   TO  DC-BIN-DATE-1
072110          MOVE SPACES            TO  DC-OPTION-CODE
072110          PERFORM 8500-DATE-CONVERSION
072110          IF DC-ERROR-CODE EQUAL TO SPACES
072110             MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-FROM-4
072110          END-IF
072110       END-IF.
072110       IF AT-PAID-THRU-DT GREATER THAN WRK-SAVE-THRU-4
072110          MOVE AT-PAID-THRU-DT   TO  WRK-SAVE-THRU-4
072110          MOVE AT-PAID-THRU-DT   TO  DC-BIN-DATE-1
072110          MOVE SPACES            TO  DC-OPTION-CODE
072110          PERFORM 8500-DATE-CONVERSION
072110          IF DC-ERROR-CODE EQUAL TO SPACES
072110             MOVE DC-GREG-DATE-A-EDIT  TO  WRK-PAID-THRU-4
072110          END-IF
072110       END-IF.
072110       GO TO 4200-TRLR-READ-LOOP.
072110       
072110       
072110 4200-EXIT.
072110     EXIT.
072110

       5000-MOVE-NAME SECTION. COPY ELCMNS.

013017 6000-load-ach-table.
013017
013017     IF NOT CONNECTED-TO-DB
013017        PERFORM 6010-prepare-sql thru 6010-exit
013017     end-if
013017
013017     move cl-carrier             to ext-carrier
013017     move cl-cert-grouping       to ext-group
013017     move cl-cert-state          to ext-state
013017     move cl-cert-account        to ext-account
013017     move cl-cert-eff-dt         to dc-bin-date-1
013017     move ' '                    to dc-option-code
013017     perform 8500-date-conversion
013017     if no-conversion-error
013017        move dc-greg-date-a-edit to ext-cert-eff-dt
013017     end-if
013017     move cl-cert-no             to ext-cert-no
013017     move cl-claim-no            to ext-claim-no
013017     move cl-insured-last-name   to ext-last-name
013017     move cl-insured-1st-name    to ext-first-name
013017     move cl-insured-mid-init    to ext-mid-init
013017     move cl-claim-type          to ext-claim-type
013017     move m420c-amount-paid      to ext-amt-paid
013017     move WRK-CHECK-DATE         to ext-paid-dt
013017     move WRK-PAID-FROM-DT       to ext-paid-from-dt
013017     move wrk-paid-thru-dt       to ext-paid-thru-dt
013017     move m420c-draft            to ext-check-no
013017     move M420C-PAYMENT-TYPE     TO ext-PAY-TYPE
013017
013017     if m420c-member-address1 = spaces
013017        move m420c-member-address2
013017                                 to m420c-member-address1
013017        move m420c-member-address3
013017                                 to m420c-member-address2
013017        move spaces              to m420c-member-address3
013017        if m420c-member-address1 = spaces
013017           move m420c-member-address2 to m420c-member-address1
013017           move m420c-member-address3 to m420c-member-address2
013017           move spaces              to m420c-member-address3
013017        end-if
013017     end-if
013017     if m420c-member-address2 = spaces
013017        move m420c-member-address3 to m420c-member-address2
013017        move spaces              to m420c-member-address3
013017     end-if
013017
013017     MOVE M420C-MEMBER-NAME      to ext-member-NAME      
013017     move m420c-member-address1  to ext-member-address1
013017     move m420c-member-address2  to ext-member-address2
013017
013017     if m420c-member-address4 not = spaces
013017        perform varying s1 from +30 by -1 until
013017           m420c-member-address4 (s1:1) not = space
013017        end-perform
013017        move m420c-member-address4 (s1 - 1:2)
013017                                 to ext-member-state
013017        move m420c-member-address4 (1:s1 - 3)
013017                                 to ext-member-city
013017     end-if
013017
013017     MOVE M420C-MEMBER-ZIP-CODE  to ext-member-ZIP-CODE  
013017
013017     if m420c-payee-address1 = spaces
013017        move m420c-payee-address2
013017                                 to m420c-payee-address1
013017        move m420c-payee-address3
013017                                 to m420c-payee-address2
013017        move spaces              to m420c-payee-address3
013017        if m420c-payee-address1 = spaces
013017           move m420c-payee-address2 to m420c-payee-address1
013017           move m420c-payee-address3 to m420c-payee-address2
013017           move spaces              to m420c-payee-address3
013017        end-if
013017     end-if
013017     if m420c-payee-address2 = spaces
013017        move m420c-payee-address3 to m420c-payee-address2
013017        move spaces              to m420c-payee-address3
013017     end-if
013017
013017     MOVE M420C-payee-NAME      to ext-payee-NAME      
013017     move m420c-payee-address1  to ext-payee-addr1
013017     move m420c-payee-address2  to ext-payee-addr2
013017
013017     if m420c-payee-address4 not = spaces
013017        perform varying s1 from +30 by -1 until
013017           m420c-payee-address4 (s1:1) not = space
013017        end-perform
013017        move m420c-payee-address4 (s1 - 1:2)
013017                                 to ext-payee-state
013017        move m420c-payee-address4 (1:s1 - 3)
013017                                 to ext-payee-city
013017     end-if
013017
013017     move M420C-PAYEE-zip-code   to ext-payee-zip
013017
013017     move cl-beneficiary         to ext-bene
013017
013017     move be-ach-aba-routing-number
013017                                 to ext-ach-aba-no
013017     move be-ach-bank-account-number
013017                                 to ext-ach-acct-no
013017
013017*    move '1111111111'           to ext-ach-aba-no
013017*    move '22222222222222222'    to ext-ach-acct-no
013017
013017     perform 6050-insert-row     thru 6050-exit
013017
013017     .
013017 6000-exit.
013017     exit.
013017
013017 6010-prepare-sql.
013017
013017     display ' about to connect to Logic '


      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate
       

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

013017
013017     if sqlcode not = 0
013017        display "Error: cannot connect to Logic"
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display sqlerrmc
013017        perform abend-pgm
013017     end-if
013017
013017     set connected-to-db to true
013017
013017*    perform 6020-drop-table     thru 6020-exit
013017*    perform 6040-create-table   thru 6040-exit
013017
013017     perform 6025-check-for-rerun thru 6025-exit
013017     if ws-rec-cntr > 0
013017        display ' This must be a re-run, about to delete '
013017        perform 6060-delete-rows thru 6060-exit
013017     end-if
013017
013017     move ws-moe-date            to ext-proc-dt
013017
013017     .
013017 6010-exit.
013017     exit.
013017
013017 6020-drop-table.
013017
013017     display 'Begin Drop table'
013017     if dte-client = 'DCC'
013017        EXEC SQL
013017           drop table DCC_CLM_PMTS_ACH
013017        END-EXEC
013017     ELSE
013017        EXEC SQL
013017           drop table CLM_PMTS_ACH
013017        END-EXEC
013017     END-IF
013017     if sqlcode not = 0
013017        display "Error(anticipated) : cannot drop table "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' message ' sqlerrmc
013017     end-if
013017
013017     .
013017 6020-exit.
013017     exit.
013017
013017 6025-check-for-rerun.
013017
013017     DISPLAY ' TEST DATE ' ws-test-date
013017
013017     if dte-client = 'DCC'
013017        exec sql
013017           SELECT
013017              PROC_DATE,
013017              Count(*)
013017           INTO
013017              :EXT-PROC-DT,
013017              :WS-REC-CNTR
013017           FROM
013017              DCC_CLM_PMTS_ACH
013017           GROUP BY PROC_DATE
013017           HAVING PROC_DATE = :ws-test-date
013017        end-exec
013017     ELSE
013017        exec sql
013017           SELECT
013017              PROC_DATE,
013017              Count(*)
013017           INTO
013017              :EXT-PROC-DT,
013017              :WS-REC-CNTR
013017           FROM
013017              CLM_PMTS_ACH
013017           GROUP BY PROC_DATE
013017           HAVING PROC_DATE = :ws-test-date
013017        end-exec
013017     end-if
013017
013017     if sqlcode not = 0 and 1 and 100
013017        display "Error : check for rerun  "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' message ' sqlerrmc
013017     end-if
013017
013017     display ' counter ' ws-rec-cntr
013017
013017     .
013017 6025-exit.
013017     exit.
013017
013017 6030-truncate-table.
013017
013017     display 'Begin Truncate table'
NTTDel*    EXEC SQL
NTTDel*        truncate table CLM_PMTS_ACH
NTTDel*    END-EXEC
NTTIns     EXEC SQL
NTTIns         EXECUTE IMMEDIATE 'truncate table CLM_PMTS_ACH'
NTTIns     END-EXEC
013017
013017     if sqlcode not = 0
013017        display "Error : cannot truncate table "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' message ' sqlerrmc
013017     end-if
013017
013017     .
013017 6030-exit.
013017     exit.
013017
013017 6040-create-table.
013017
013017     display ' Begin Create table '
013017     IF DTE-CLIENT = 'DCC'
013017     EXEC SQL
013017        CREATE TABLE DCC_CLM_PMTS_ACH(
013017           PROC_DATE          datetime NOT NULL,
013017           BENEFICIARY        char(10) not null,
013017          CARRIER            char(1) NOT NULL,
013017          GROUPING           char(6) NOT NULL,
013017          STATE              char(2) NOT NULL,
013017          ACCOUNT            char(10) NOT NULL,
013017          EFF_DATE           datetime NOT NULL,
013017          CERT_NO            char(11) NOT NULL,
013017          CLAIM_NO           char(7) not NULL,
013017          CLM_LNAME          varchar(15) NULL,
013017          CLM_FNAME          varchar(10) NULL,
013017          CLM_INIT           char(1) NULL,
013017          MEMBER_NAME        varchar(30) NULL,
013017          MEMBER_ADDRESS1    varchar(30) NULL,
013017          MEMBER_ADDRESS2    varchar(30) NULL,
013017          MEMBER_CITY        varchar(30) NULL,
013017          MEMBER_STATE       char(2) NULL,
013017          MEMBER_ZIP         varchar(9) NULL,
013017          CLM_TYPE           char(1) NULL,
013017          AMT_PAID           decimal(9,2) NULL,
013017          PAY_DATE           datetime NULL,
013017          PAID_FROM_DATE     datetime NULL,
013017          PAID_THRU_DATE     datetime NULL,
013017          CHECK_NO           char(10) NULL,
013017          PAY_CODE           char(1) NULL,
013017          PAYEE_NAME         varchar(30) NULL,
013017          PAYEE_ADDRESS1     varchar(30) NULL,
013017          PAYEE_ADDRESS2     varchar(30) NULL,
013017          PAYEE_CITY         varchar(30) NULL,
013017          PAYEE_STATE        char(2) NULL,
013017          PAYEE_ZIP          varchar(9) NULL,
013017          ABA_NUM            char(15) not null,
013017          ACCT_NUM           char(20) not null,
013017          MEMBER_ID          varchar(18) NULL,
013017          LOAN_NBR           varchar(6) null
013017           )                                             
013017     END-EXEC                                              
013017     ELSE
013017     EXEC SQL
013017        CREATE TABLE CLM_PMTS_ACH(
013017           PROC_DATE          datetime NOT NULL,
013017           BENEFICIARY        char(10) not null,
013017          CARRIER            char(1) NOT NULL,
013017          GROUPING           char(6) NOT NULL,
013017          STATE              char(2) NOT NULL,
013017          ACCOUNT            char(10) NOT NULL,
013017          EFF_DATE           datetime NOT NULL,
013017          CERT_NO            char(11) NOT NULL,
013017          CLAIM_NO           char(7) not NULL,
013017          CLM_LNAME          varchar(15) NULL,
013017          CLM_FNAME          varchar(10) NULL,
013017          CLM_INIT           char(1) NULL,
013017          MEMBER_NAME        varchar(30) NULL,
013017          MEMBER_ADDRESS1    varchar(30) NULL,
013017          MEMBER_ADDRESS2    varchar(30) NULL,
013017          MEMBER_CITY        varchar(30) NULL,
013017          MEMBER_STATE       char(2) NULL,
013017          MEMBER_ZIP         varchar(9) NULL,
013017          CLM_TYPE           char(1) NULL,
013017          AMT_PAID           decimal(9,2) NULL,
013017          PAY_DATE           datetime NULL,
013017          PAID_FROM_DATE     datetime NULL,
013017          PAID_THRU_DATE     datetime NULL,
013017          CHECK_NO           char(10) NULL,
013017          PAY_CODE           char(1) NULL,
013017          PAYEE_NAME         varchar(30) NULL,
013017          PAYEE_ADDRESS1     varchar(30) NULL,
013017          PAYEE_ADDRESS2     varchar(30) NULL,
013017          PAYEE_CITY         varchar(30) NULL,
013017          PAYEE_STATE        char(2) NULL,
013017          PAYEE_ZIP          varchar(9) NULL,
013017          ABA_NUM            char(15) not null,
013017          ACCT_NUM           char(20) not null,
013017          MEMBER_ID          varchar(18) NULL,
013017          LOAN_NBR           varchar(6) null
013017           )                                             
013017     END-EXEC                                              
013017     END-IF
013017*          CONSTRAINT PK_CLM_PMTS_ACH PRIMARY KEY CLUSTERED
013017*            (PROC_DATE, CARRIER, GROUPING, STATE, ACCOUNT,
013017*            EFF_DATE, CERT_NO, CLAIM_NO)
013017
013017     if sqlcode not = 0
013017        display "Error: cannot create table "
013017        move sqlcode             to ws-disp-code
013017        display ' sql return code ' ws-disp-code
013017        display ' sql err mess    ' sqlerrmc
013017        PERFORM 6070-FINISH-UP   THRU 6070-EXIT
013017        PERFORM ABEND-PGM
013017     end-if
013017
013017     .
013017 6040-exit.
013017     exit.
013017
013017 6050-INSERT-ROW.
013017
013017     display ' about to insert ' 
013017
013017     IF DTE-CLIENT = 'DCC'
013017     EXEC SQL
013017        insert into DCC_CLM_PMTS_ACH (
013017           PROC_DATE         ,
013017           BENEFICIARY       ,
013017           CARRIER           ,
013017           GROUPING          ,
013017           STATE             ,
013017           ACCOUNT           ,
013017           EFF_DATE          ,
013017           CERT_NO           ,
013017           CLAIM_NO          ,
013017           CLM_LNAME         ,
013017           CLM_FNAME         ,
013017           CLM_INIT          ,
013017           MEMBER_NAME       ,
013017           MEMBER_ADDRESS1   ,
013017           MEMBER_ADDRESS2   ,
013017           MEMBER_CITY       ,
013017           MEMBER_STATE      ,
013017           MEMBER_ZIP        ,
013017           CLM_TYPE          ,
013017           AMT_PAID          ,
013017           PAY_DATE          ,
013017           PAID_FROM_DATE    ,
013017           PAID_THRU_DATE    ,
013017           CHECK_NO          ,
013017           PAY_CODE          ,
013017           PAYEE_NAME        ,
013017           PAYEE_ADDRESS1    ,
013017           PAYEE_ADDRESS2    ,
013017           PAYEE_CITY        ,
013017           PAYEE_STATE       ,
013017           PAYEE_ZIP         ,
013017           ABA_NUM           ,
013017           ACCT_NUM          ,
082219           CASHED_DATE       ,
082219           ACCOUNT_NAME)
013017         values (
013017           :ext-proc-dt      ,
013017           :ext-bene         ,
013017           :ext-carrier      ,
013017           :ext-group        ,
013017           :ext-state        ,
013017           :ext-account      ,
013017           :ext-cert-eff-dt  ,
013017           :ext-cert-no      ,
013017           :ext-claim-no     ,
013017           :ext-last-name    ,
013017           :ext-first-name   ,
013017           :ext-mid-init     ,
013017           :ext-MEMBER-NAME     ,
013017           :ext-MEMBER-ADDRESS1 ,
013017           :ext-MEMBER-ADDRESS2 ,
013017           :ext-MEMBER-CITY  ,
013017           :ext-MEMBER-STATE ,
013017           :ext-MEMBER-ZIP-CODE ,
013017           :ext-claim-type   ,
013017           :ext-amt-paid-a   ,
013017           :ext-paid-dt      ,
013017           :ext-paid-from-dt ,
013017           :ext-paid-thru-dt ,
013017           :ext-check-no     ,
013017           :ext-pay-type     ,
013017           :ext-payee-name   ,
013017           :ext-payee-addr1  ,
013017           :ext-payee-addr2  ,
013017           :ext-payee-CITY   ,
013017           :ext-payee-STATE  ,
013017           :ext-payee-zip    ,
013017           :ext-ach-aba-no   ,
013017           :ext-ach-acct-no  ,
082219           :ws-ach-cashed-date,
082219           :ext-account-name)
013017     END-EXEC
013017     ELSE
013017     EXEC SQL
013017        insert into CLM_PMTS_ACH (
013017           PROC_DATE         ,
013017           BENEFICIARY       ,
013017           CARRIER           ,
013017           GROUPING          ,
013017           STATE             ,
013017           ACCOUNT           ,
013017           EFF_DATE          ,
013017           CERT_NO           ,
013017           CLAIM_NO          ,
013017           CLM_LNAME         ,
013017           CLM_FNAME         ,
013017           CLM_INIT          ,
013017           MEMBER_NAME       ,
013017           MEMBER_ADDRESS1   ,
013017           MEMBER_ADDRESS2   ,
013017           MEMBER_CITY       ,
013017           MEMBER_STATE      ,
013017           MEMBER_ZIP        ,
013017           CLM_TYPE          ,
013017           AMT_PAID          ,
013017           PAY_DATE          ,
013017           PAID_FROM_DATE    ,
013017           PAID_THRU_DATE    ,
013017           CHECK_NO          ,
013017           PAY_CODE          ,
013017           PAYEE_NAME        ,
013017           PAYEE_ADDRESS1    ,
013017           PAYEE_ADDRESS2    ,
013017           PAYEE_CITY        ,
013017           PAYEE_STATE       ,
013017           PAYEE_ZIP         ,
013017           ABA_NUM           ,
013017           ACCT_NUM          ,
082219           CASHED_DATE       ,
082219           ACCOUNT_NAME)
013017         values (
013017           :ext-proc-dt      ,
013017           :ext-bene         ,
013017           :ext-carrier      ,
013017           :ext-group        ,
013017           :ext-state        ,
013017           :ext-account      ,
013017           :ext-cert-eff-dt  ,
013017           :ext-cert-no      ,
013017           :ext-claim-no     ,
013017           :ext-last-name    ,
013017           :ext-first-name   ,
013017           :ext-mid-init     ,
013017           :ext-MEMBER-NAME     ,
013017           :ext-MEMBER-ADDRESS1 ,
013017           :ext-MEMBER-ADDRESS2 ,
013017           :ext-MEMBER-CITY  ,
013017           :ext-MEMBER-STATE ,
013017           :ext-MEMBER-ZIP-CODE ,
013017           :ext-claim-type   ,
013017           :ext-amt-paid-a   ,
013017           :ext-paid-dt      ,
013017           :ext-paid-from-dt ,
013017           :ext-paid-thru-dt ,
013017           :ext-check-no     ,
013017           :ext-pay-type     ,
013017           :ext-payee-name   ,
013017           :ext-payee-addr1  ,
013017           :ext-payee-addr2  ,
013017           :ext-payee-CITY   ,
013017           :ext-payee-STATE  ,
013017           :ext-payee-zip    ,
013017           :ext-ach-aba-no   ,
013017           :ext-ach-acct-no  ,
082219           :ws-ach-cashed-date,
082219           :ext-account-name)
013017     END-EXEC
013017     END-IF
013017
013017     if sqlcode not = 0
013017        display "Error: cannot insert row "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' sql err mess    ' sqlerrmc
013017        display ' offending rec ' extract-record
013017        PERFORM 6070-FINISH-UP   THRU 6070-EXIT
013017        PERFORM ABEND-PGM
013017     end-if
013017
013017     add 1 to ws-records-inserted
013017
013017     .
013017 6050-EXIT.
013017     EXIT.
013017
013017 6060-delete-rows.
013017
013017     IF DTE-CLIENT = 'DCC'
013017     exec sql delete
013017        FROM
013017           DCC_CLM_PMTS_ACH
013017        where PROC_DATE = :ws-test-date
013017     end-exec
013017     ELSE
013017     exec sql delete
013017        FROM
013017           CLM_PMTS_ACH
013017        where PROC_DATE = :ws-test-date
013017     end-exec
013017     END-IF
013017
013017     if sqlcode not = 0 and 1
013017        display "Error : deleting rows  "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' message ' sqlerrmc
013017        go to 6060-exit
013017     end-if
013017
013017     EXEC SQL
013017         commit
013017     END-EXEC
013017
013017     if sqlcode not = 0 and 1
013017        display "Error : commit of delete  "
013017        move sqlcode             to ws-sql-code
013017        move ws-sql-code         to ws-dis-sql-code
013017        display ' sqlcode ' ws-dis-sql-code
013017        display ' message ' sqlerrmc
013017     end-if
013017
013017     .
013017 6060-exit.
013017     exit.
013017
013017 6070-finish-up.

082219* It's possible that there are no records to process
082219* and the program never connected to the DB to begin with
082219
082219     IF NOT CONNECTED-TO-DB
082219        go to 6070-exit
082219     end-if

NTTDel*    EXEC SQL
NTTDel*        commit work release
NTTDel*    END-EXEC
NTTIns     EXEC SQL
NTTIns         commit
NTTIns     END-EXEC
013017
013017     if sqlcode not = 0
013017        display "Error: commit work release "
013017        display ' sql return code ' sqlcode
013017        display ' sql err mess    ' sqlerrmc
013017     end-if
013017
013017     EXEC SQL
013017        DISCONNECT
013017     END-EXEC
013017
013017     if sqlcode not = 0
013017        display "Error: disconnect  "
013017        display ' sql return code ' sqlcode
013017        display ' sql err mess    ' sqlerrmc
013017     end-if
013017
013017     .
013017 6070-exit.
           exit.

       8500-DATE-CONVERSION.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

       8700-LOCATE-BENEFIT.

           MOVE SPACES                 TO  WS-KIND.
032311
032311     START ELCNTL KEY >= CF-CONTROL-PRIMARY           
032311     EVALUATE CNTL-STATUS
032311       WHEN '00'
032311         CONTINUE
032311       WHEN '23'
032311         EXIT PARAGRAPH
032311       WHEN OTHER
032311         MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
032311         MOVE 'READ ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
032311         PERFORM ABEND-PGM
032311     END-EVALUATE.

           READ ELCNTL NEXT
           EVALUATE CNTL-STATUS
             WHEN '00'
               CONTINUE
             WHEN '23'
               EXIT PARAGRAPH
             WHEN OTHER
               MOVE CNTL-STATUS TO WS-ABEND-FILE-STATUS
               MOVE 'READ ERROR ON ELCNTL FILE' TO WS-ABEND-MESSAGE
               PERFORM ABEND-PGM
           END-EVALUATE.

           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 8
032311       IF WRK-HOLD-BENEFIT-CD = CF-BENEFIT-CODE (WS-INDEX)
                 MOVE CF-BENEFIT-ALPHA    (WS-INDEX) TO WS-KIND
                 MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO WS-COV-TYPE
                 EXIT PERFORM
             END-IF

             IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
                 EXIT PERFORM
             END-IF
           END-PERFORM.

       ABEND-PGM SECTION.
           MOVE +999 TO WS-RETURN-CODE
                        ws-goback-ret-code
                                COPY ELCABEND.

       9999-END-OF-JOB.

           PERFORM 6070-FINISH-UP   THRU 6070-EXIT
           CLOSE MICR-DRAFT-FILE
           IF MICR-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' MICR-STATUS ' ON MICRDRFT FILE'
           END-IF

           CLOSE ELCHKQ2
           IF CHKQ-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' CHKQ-STATUS ' ON ELCHKQ2 FILE'
           END-IF

           CLOSE ELCNTL
           IF CNTL-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' CNTL-STATUS ' ON ELCNTL FILE'
           END-IF

           CLOSE ELTRLR
           IF TRLR-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' TRLR-STATUS ' ON ELTRLR FILE'
           END-IF

           CLOSE ELCERT
           IF CERT-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' CERT-STATUS ' ON ELCERT FILE'
           END-IF

           CLOSE ELMSTR
           IF MSTR-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' MSTR-STATUS ' ON ELMSTR FILE'
           END-IF

           CLOSE ERACCT
           IF ACCT-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' ACCT-STATUS ' ON ERACCT FILE'
           END-IF

           CLOSE ELBENE
           IF BENE-STATUS NOT = '00'
              DISPLAY 'CLOSE ERROR ' BENE-STATUS ' ON ELBENE FILE'
           END-IF

072110     CLOSE NAPERSOFT-LETTERS.
072110
           GOBACK.

