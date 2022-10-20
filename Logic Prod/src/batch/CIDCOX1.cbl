       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCOX1.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
      *REMARKS.
030404******************************************************************
030404*                   C H A N G E   L O G
030404*
030404* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030404*-----------------------------------------------------------------
030404*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030404* EFFECTIVE    NUMBER
030404*-----------------------------------------------------------------
030404* 030404                   SMVA  ADD PROCESSING FOR DCC
052704* 052704  IR2004052400001  SMVA  CHG DELIMITER FROM TAB TO ;
083105* 083105  CR2005031100005  PEMA  ADD FIELDS TO EXTRACT
011807* 011807                   PEMA  ADD DATEFILE PROCESSING
100208* 100208  CR2008090200004  PEMA  ADD NEW FIELDS TO EXTRACT
081610* 081610  IR2010081200002  AJRA  USE SEPARATED CITY STATE
073112* 073112  CR2012052500003  PEMA  ADD FIELDS TO EXTRACT
072718* 072718  CR2018070500003  PEMA  ADD OVR-UNDR TO EXTRACT
072320* 072320  CR2020060800001  PEMA  Add all fields to extract
030404******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT COMP-OUT     ASSIGN TO COMPOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  COMP-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

072320 01  COMP-OUT-REC                PIC X(1600).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERCOMP.

                                       COPY ERCCOMP.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCOX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

030404 01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERCOMP                  VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX     VALUE '00'.
072320     05  ws-work-century         pic 99     value zeros.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.

           05  WS-WORK-CITY-ST.
               10  WS-BYTE OCCURS 29   PIC X.
           05  WS-ERCOMP-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT           PIC 9(7)   VALUE ZEROS.
072320     05  ws-work-time            pic 9(7).
072320     05  filler redefines ws-work-time.
072320         10  filler              pic x.
072320         10  ws-hh               pic 99.
072320         10  ws-mm               pic 99.
072320         10  ws-ss               pic 99.


072320 01  WS-SAVE-ERCOMP              PIC X(1000) VALUE LOW-VALUES.
       01  ERCOMP-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-RESP-NO              PIC X(10).
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TYPE                 PIC X.
072320     12  EX-STMT-TYPE            pic xxx.
072320     12  ex-comp-spp             pic x.
           12  EX-BALANCE-CONTROL      PIC X.
072320     12  EX-INTERNAL-CONTROL-1   PIC X.
072320     12  EX-INTERNAL-CONTROL-2   PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-MAIL-NAME            PIC X(30).
           12  EX-ADDR-1               PIC X(30).
           12  EX-ADDR-2               PIC X(30).
           12  EX-ADDR-3               PIC X(29).
           12  EX-CSO-1099             PIC X.
           12  EX-ZIP                  PIC 9(9).
           12  EX-SOC-SEC              PIC X(13).
           12  EX-TELEPHONE            PIC X(10).
072320     12  EX-ROLODEX-PRINT-DT     PIC X(10).
072320     12  EX-AR-BALANCE-LEVEL     PIC X.
072320     12  EX-AR-NORMAL-PRINT      PIC X.
072320     12  EX-AR-SUMMARY-CODE      PIC X(6).
072320     12  EX-AR-REPORTING         PIC X.
072320     12  EX-AR-PULL-CHECK        PIC X.
072320     12  EX-AR-BAL-PRINT         PIC X.
072320     12  EX-AR-LAST-RUN-CODE     PIC X.
072320     12  EX-LAST-EOM-STMT-DT     PIC X(10).
072320     12  EX-USER-CODE            PIC X.
072320     12  EX-LAST-ACTIVITY-DT     PIC X(10).
072320     12  EX-LAST-STMT-DT         PIC X(10).
           12  EX-BAL-FWD              PIC -9(7).99.
           12  EX-CUR-COM              PIC -9(7).99.
           12  EX-CUR-CHG              PIC -9(7).99.
           12  EX-CUR-PMT              PIC -9(7).99.
           12  EX-END-BAL              PIC -9(7).99.
           12  EX-YTD-COM              PIC -9(7).99.
           12  EX-YTD-OV               PIC -9(7).99.
           12  EX-CSR-CODE             PIC X(4).
           12  EX-FAXNO                PIC X(10).
           12  EX-BILL-SW              PIC X.
           12  EX-CONTROL-NAME         PIC X(30).
           12  EX-STATE                PIC XX.
           12  EX-STATUS-CODE          PIC X.
           12  EX-EFF-DT               PIC X(10).
           12  EX-EXP-DT               PIC X(10).
           12  EX-COMMENT-1            PIC X(40).
           12  EX-COMMENT-2            PIC X(40).
           12  EX-COMMENT-3            PIC X(40).
           12  EX-COMMENT-4            PIC X(40).
083105     12  EX-CLP-STATE            PIC XX.
           12  EX-BANK-FEE             PIC -9(5).99.
           12  EX-BANK-FEE-LEASE       PIC -9(5).99.
           12  EX-LAST-MAINT-USER      PIC XXXX.
           12  EX-LAST-MAINT-DT        PIC X(10).
072320     12  EX-LAST-MAINT-TIME      pic x(8).
           12  EX-AGE-CURRENT          PIC -9(7).99.
           12  EX-AGE-OVER30           PIC -9(7).99.
           12  EX-AGE-OVER60           PIC -9(7).99.
           12  EX-AGE-OVER90           PIC -9(7).99.
           12  EX-STMT-OWNER           PIC XXXX.
           12  EX-REPORT-GROUP-ID      PIC X(12).
073112     12  ex-ga-withold-pct       pic -9.9999.
073112     12  ex-ga-direct-deposit    pic x.
073112     12  ex-md-gl-acct           pic x(10).
073112     12  ex-md-div               pic xx.
073112     12  ex-md-center            pic xxxx.
073112     12  ex-md-amount            pic -9(5).99.
072718     12  ex-write-off            pic -9(7).99.
072320     12  ex-write-off-ytd        pic -9(7).99.
072320     12  ex-cur-fica             pic -9(7).99.
072320     12  ex-ytd-fica             pic -9(7).99.
072320     12  ex-lf-claim-amt         pic -9(7).99.
072320     12  ex-ah-claim-amt         pic -9(7).99.
072320     12  EX-current-LAST-STMT-DT pic X(10).
072320     12  EX-current-BAL-FWD      PIC -9(7).99.
072320     12  EX-current-CUR-COM      PIC -9(7).99.
072320     12  EX-current-CUR-CHG      PIC -9(7).99.
072320     12  EX-current-CUR-PMT      PIC -9(7).99.
072320     12  EX-current-END-BAL      PIC -9(7).99.
072320     12  EX-AGE-CURRENT-cur      PIC -9(7).99.
072320     12  EX-AGE-current-OVER30   PIC -9(7).99.
072320     12  EX-AGE-current-OVER60   PIC -9(7).99.
072320     12  EX-AGE-current-OVER90   PIC -9(7).99.
072320     12  EX-current-YTD-COM      PIC -9(7).99.
072320     12  EX-current-YTD-OV       PIC -9(7).99.                   
072320     12  ex-ytd-paid-comm        pic -9(7).99.
072320     12  ex-ytd-paid-ov          pic -9(7).99.
072320     12  ex-current-month-activity
072320                                 pic x.
072320     12  ex-delinquent-letter-cd pic x.
072320     12  ex-report-code-2        pic x(10).
072320     12  ex-ahl-age-over120      pic -9(7).99.
072320     12  ex-ahl-age-current-over120
072320                                 pic -9(7).99.
072320     12  ex-type-agent           pic x.
072320     12  ex-create-ap-check      pic x.
072320     12  ex-deliver-to-mel       pic x.
072320     12  ex-ach-status           pic x.
072320     12  ex-first-written-dt     pic x(10).
072320     12  ex-spp-refund-edit      pic x.
           12  EX-EOR                  PIC X.

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


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ERCOMP)
PEMTST*         OR (WS-ERCOMP-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERCOMP-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERCOMP-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCOMP
               OUTPUT COMP-OUT

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP OPEN ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOMP COMP-OUT

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP CLOSE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERCOMP-DETAIL-RECORD
           MOVE 'E'                    TO EX-EOR
           MOVE '**'                   TO EX-STATE
072320     move zeros                  to EX-BAL-FWD
072320                                    EX-CUR-COM               
072320                                    EX-CUR-CHG               
072320                                    EX-CUR-PMT               
072320                                    EX-END-BAL               
072320                                    EX-YTD-COM               
072320                                    EX-YTD-OV                
072320                                    EX-CSR-CODE              
072320                                    EX-BANK-FEE              
072320                                    EX-BANK-FEE-LEASE        
072320                                    EX-AGE-CURRENT           
072320                                    EX-AGE-OVER30            
072320                                    EX-AGE-OVER60            
072320                                    EX-AGE-OVER90            
072320                                    ex-ga-withold-pct        
072320                                    ex-md-amount             
072320                                    ex-write-off             
072320                                    ex-write-off-ytd         
072320                                    ex-cur-fica              
072320                                    ex-ytd-fica              
072320                                    ex-lf-claim-amt          
072320                                    ex-ah-claim-amt          
072320                                    EX-current-BAL-FWD       
072320                                    EX-current-CUR-COM       
072320                                    EX-current-CUR-CHG       
072320                                    EX-current-CUR-PMT       
072320                                    EX-current-END-BAL       
072320                                    EX-AGE-CURRENT-cur       
072320                                    EX-AGE-current-OVER30    
072320                                    EX-AGE-current-OVER60    
072320                                    EX-AGE-current-OVER90    
072320                                    EX-current-YTD-COM       
072320                                    EX-current-YTD-OV        
072320                                    ex-ytd-paid-comm         
072320                                    ex-ytd-paid-ov           
072320                                    ex-ahl-age-over120       
072320                                    ex-ahl-age-current-over120

           MOVE ERCOMP-DETAIL-RECORD   TO WS-SAVE-ERCOMP

072320     string
072320        'EX_CARRIER'                 ';'
072320        'EX_GROUPING'                ';'
072320        'EX_RESP_NO'                 ';'
072320        'EX_ACCOUNT'                 ';'
072320        'EX_TYPE'                    ';'
072320        'EX_STMT_TYPE'               ';'
072320        'EX_COMP_SPP'                ';'
072320        'EX_BALANCE_CONTROL'         ';'
072320        'EX_INTERNAL_CONTROL1'       ';'
072320        'EX_INTERNAL_CONTROL2'       ';'
072320        'EX_ACCOUNT_NAME'            ';'
072320        'EX_CONTRACTED_NAME'         ';'
072320        'EX_ADDR_1'                  ';'
072320        'EX_ADDR_2'                  ';'
072320        'EX_ADDR_3'                  ';'
072320        'EX_CSO_1099'                ';'
072320        'EX_ZIP'                     ';'
072320        'EX_SOC_SEC'                 ';'
072320        'EX_TELEPHONE'               ';'
072320        'EX_ROLODEX_PRINT_DT'        ';'
072320        'EX_AR_BAL_LEVEL'            ';'
072320        'EX_AR_NORMAL_PRINT'         ';'
072320        'EX_AR_SUMMARY_CODE'         ';'
072320        'EX_AR_REPORTING'            ';'
072320        'EX_AR_PULL_CHECK'           ';'
072320        'EX_AR_BAL_PRINT'            ';'
072320        'EX_AR_LAST_RUN_CODE'        ';'
072320        'EX_AR_LAST_EOM_STMT_DT'     ';'
072320        'EX_USER_CODE'               ';'
072320        'EX_LAST_ACTIVITY_DT'        ';'
072320        'EX_LAST_STMT_DT'            ';'
072320        'EX_BAL_FWD'                 ';'
072320        'EX_CUR_COM'                 ';'
072320        'EX_CUR_CHG'                 ';'
072320        'EX_CUR_PMT'                 ';'
072320        'EX_END_BAL'                 ';'
072320        'EX_YTD_COM'                 ';'
072320        'EX_YTD_OV'                  ';'
072320        'EX_CSR_CODE'                ';'
072320        'EX_FAXNO'                   ';'
072320        'EX_BILL_SW'                 ';'
072320        'EX_CONTROL_NAME'            ';'
072320        'EX_STATE'                   ';'
072320        'EX_STATUS'                  ';'
072320        'EX_EFFECT'                  ';'
072320        'EX_EXPIRE'                  ';'
072320        'EX_COMMENT1'                ';'
072320        'EX_COMMENT2'                ';'
072320        'EX_COMMENT3'                ';'
072320        'EX_COMMENT4'                ';'
072320        'EX_CLP_STATE'               ';'
072320        'EX_BANK_FEE'                ';'
072320        'EX_BANK_FEE_LEASE'          ';'
072320        'EX_LAST_MAINT_USER'         ';'
072320        'EX_LAST_MAINT_DT'           ';'
072320        'EX_LAST_MAINT_TIME'         ';'
072320        'EX_AGE_CURRENT'             ';'
072320        'EX_AGE_OVER30'              ';'
072320        'EX_AGE_OVER60'              ';'
072320        'EX_AGE_OVER90'              ';'
072320        'EX_STMT_OWNER'              ';'
072320        'EX_REPORT_GROUP_ID'         ';'
072320        'EX_GA_WITHHOLD_PCT'         ';'
072320        'EX_GA_DIRECT_DEPOSIT'       ';'
072320        'EX_MD_GL_ACCT'              ';'
072320        'EX_MD_DIV'                  ';'
072320        'EX_MD_CENTER'               ';'
072320        'EX_MD_AMOUNT'               ';'
072320        'EX_WRITE_OFF'               ';'
072320        'EX_WRITE_OFF_YTD'           ';'
072320        'EX_CUR_FICA'                ';'
072320        'EX_YTD_FICA'                ';'
072320        'EX_LF_CLM_AMT'              ';'
072320        'EX_AH_CLM_AMT'              ';'
072320        'EX_CURRENT_LAST_STMT_DT'    ';'
072320        'EX_CURRENT_BAL_FWD'         ';'
072320        'EX_CURRENT_CUR_COM'         ';'
072320        'EX_CURRENT_CUR_CHG'         ';'
072320        'EX_CURRENT_CUR_PMT'         ';'
072320        'EX_CURRENT_END_BAL'         ';'
072320        'EX_AGE_CURRENT_CUR'         ';'
072320        'EX_AGE_CURRENT_OVER30'      ';'
072320        'EX_AGE_CURRENT_OVER60'      ';'
072320        'EX_AGE_CURRENT_OVER90'      ';'
072320        'EX_CURRENT_YTD_COM'         ';'
072320        'EX_CURRENT_YTD_OV'          ';'
072320        'EX_YTD_PAID_COM'            ';'
072320        'EX_YTD_PAID_OV'             ';'
072320        'EX_CURRENT_MONTH_ACTIVITY'  ';'
072320        'EX_DELINQUENT_LETTER_CD'    ';'
072320        'EX_REPORT_CODE_2'           ';'
072320        'EX_AHL_AGE_OVER120'         ';'
072320        'EX_AHL_AGE_CURRENT_OVER120' ';'
072320        'EX_TYPE_AGENT'              ';'
072320        'EX_CREATE_AP_CHECK'         ';'
072320        'EX_DELIVER_TO_MEL'          ';'
072320        'EX_ACH_STATUS'              ';'
072320        'EX_FIRST_WRITTEN_DT'        ';'
072320        'EX_SPP_REFUND_EDIT'         ';'
072320        'EOR' delimited by size into comp-out-rec
072320     end-string

           write comp-out-rec

           move spaces to comp-out-rec
           PERFORM 0120-START-ERCOMP   THRU 0120-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE WS-SAVE-ERCOMP         TO ERCOMP-DETAIL-RECORD

           MOVE CO-CARRIER             TO EX-CARRIER
           MOVE CO-GROUPING            TO EX-GROUPING
           MOVE CO-RESP-NO             TO EX-RESP-NO
           IF CO-ACCOUNT = LOW-VALUES
              MOVE SPACES              TO EX-ACCOUNT
           ELSE
              MOVE CO-ACCOUNT          TO EX-ACCOUNT
           END-IF
           MOVE CO-TYPE                TO EX-TYPE

           MOVE CO-BALANCE-CONTROL     TO EX-BALANCE-CONTROL
           MOVE CO-ACCT-NAME           TO EX-ACCT-NAME
           IF CO-MAIL-NAME = LOW-VALUES
              MOVE SPACES              TO EX-MAIL-NAME
           ELSE
              MOVE CO-MAIL-NAME        TO EX-MAIL-NAME
           END-IF
           MOVE CO-ADDR-1              TO EX-ADDR-1
           MOVE CO-ADDR-2              TO EX-ADDR-2
081610*    move co-addr-3              to ws-work-city-st
081610*    PERFORM VARYING SUB1 FROM +29 BY -1 UNTIL
081610*       (SUB1 < +1)
081610*       OR (WS-BYTE (SUB1) NOT = ' ' AND '.' AND ',')
081610*    END-PERFORM
081610*    IF SUB1 > +2
081610*       MOVE WS-WORK-CITY-ST (SUB1 - 1:2)
081610*                                TO EX-STATE
081610*       MOVE WS-WORK-CITY-ST (1:SUB1 - 2)
081610*                                TO EX-ADDR-3
081610*    END-IF
081610     MOVE CO-ADDR-CITY           TO EX-ADDR-3
081610     IF CO-ADDR-STATE NOT EQUAL SPACES
081610         MOVE CO-ADDR-STATE      TO EX-STATE
081610     END-IF
           INSPECT EX-ADDR-3 REPLACING ALL ',' BY ' '
      *    MOVE CO-ADDR-3              TO EX-ADDR-3
           MOVE CO-CSO-1099            TO EX-CSO-1099
           MOVE CO-ZIP                 TO EX-ZIP
           IF CO-SOC-SEC (1:5) = LOW-VALUES
              MOVE SPACES              TO EX-SOC-SEC
           ELSE
              MOVE CO-SOC-SEC          TO EX-SOC-SEC
           END-IF
           MOVE CO-TELEPHONE           TO EX-TELEPHONE
           MOVE CO-BAL-FWD             TO EX-BAL-FWD
           MOVE CO-CUR-COM             TO EX-CUR-COM
           MOVE CO-CUR-CHG             TO EX-CUR-CHG
           MOVE CO-CUR-PMT             TO EX-CUR-PMT
           MOVE CO-END-BAL             TO EX-END-BAL
           MOVE CO-YTD-COM             TO EX-YTD-COM
           MOVE CO-YTD-OV              TO EX-YTD-OV
092208     MOVE CO-CUR                 TO EX-AGE-CURRENT
092208     MOVE CO-OV30                TO EX-AGE-OVER30
092208     MOVE CO-OV60                TO EX-AGE-OVER60
092208     MOVE CO-OV90                TO EX-AGE-OVER90
073112*    if co-ov120 not numeric
073112*       move zeros               to co-ov120
073112*    end-if
073112*    move co-ov120               to ex-age-over120
072718     if co-cur-ovr-undr not numeric
072718        move zeros               to co-cur-ovr-undr
072718     end-if
072718     move co-cur-ovr-undr        to ex-write-off
           IF CO-CSR-CODE = LOW-VALUES
              MOVE SPACES              TO EX-CSR-CODE
           ELSE
              MOVE CO-CSR-CODE         TO EX-CSR-CODE
           END-IF
           IF CO-FAXNO = LOW-VALUES
              MOVE SPACES              TO EX-FAXNO
           ELSE
              MOVE CO-FAXNO            TO EX-FAXNO
           END-IF
           MOVE CO-BILL-SW             TO EX-BILL-SW
           INSPECT CO-CONTROL-NAME
              REPLACING ALL X'0C' BY SPACES
                        ALL X'00' BY SPACES
           MOVE CO-CONTROL-NAME        TO EX-CONTROL-NAME

           MOVE CO-GA-STATUS-CODE      TO EX-STATUS-CODE

           MOVE CO-GA-EFFECTIVE-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-EFF-DT
           ELSE
              MOVE SPACES              TO EX-EFF-DT
           END-IF

           IF CO-GA-TERMINATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO EX-EXP-DT
           ELSE
              MOVE CO-GA-TERMINATION-DT
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-EXP-DT
              ELSE
                 MOVE SPACES           TO EX-EXP-DT
              END-IF
           END-IF
           MOVE CO-GA-COMMENT-1        TO EX-COMMENT-1
           MOVE CO-GA-COMMENT-2        TO EX-COMMENT-2
           MOVE CO-GA-COMMENT-3        TO EX-COMMENT-3
           MOVE CO-GA-COMMENT-4        TO EX-COMMENT-4
           IF CO-STMT-OWNER NOT = SPACES AND LOW-VALUES
              MOVE CO-STMT-OWNER       TO EX-STMT-OWNER
           END-IF
           IF CO-REPORT-GROUP-ID NOT = SPACES AND LOW-VALUES
              MOVE CO-REPORT-GROUP-ID  TO EX-REPORT-GROUP-ID
           END-IF

083105     IF DTE-CLIENT = 'DCC'
083105        IF CO-MAX-BANK-FEE NOT NUMERIC
083105           MOVE +0               TO CO-MAX-BANK-FEE
083105        END-IF
083105        IF CO-MAX-BANK-FEE-LEASE NOT NUMERIC
083105           MOVE +0               TO CO-MAX-BANK-FEE-LEASE
083105        END-IF
083105        IF CO-CLP-STATE = SPACES OR ZEROS OR LOW-VALUES
083105           MOVE SPACES           TO CO-CLP-STATE
083105        END-IF
083105        MOVE CO-CLP-STATE        TO EX-CLP-STATE
083105        MOVE CO-MAX-BANK-FEE     TO EX-BANK-FEE
083105        MOVE CO-MAX-BANK-FEE-LEASE
083105                                 TO EX-BANK-FEE-LEASE
083105     ELSE
083105        MOVE ZEROS               TO EX-BANK-FEE
083105                                    EX-BANK-FEE-LEASE
083105        MOVE SPACES              TO EX-CLP-STATE
083105     END-IF

083105     MOVE CO-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
083105     MOVE CO-LAST-MAINT-DT       TO DC-BIN-DATE-1
083105     MOVE ' '                    TO DC-OPTION-CODE
083105     PERFORM 8510-DATE-CONVERSION
083105                                 THRU 8590-EXIT
083105     IF NO-CONVERSION-ERROR
083105        MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
083105     ELSE
083105        MOVE SPACES              TO EX-LAST-MAINT-DT
083105     END-IF

073112     if co-ga-withold-pct not numeric
073112        move zeros               to co-ga-withold-pct
073112     end-if
073112
073112     if co-md-amt not numeric
073112        move zeros               to co-md-amt
073112     end-if
073112
073112     move co-ga-withold-pct      to ex-ga-withold-pct
073112     if co-ga-direct-dep = 'Y'
073112        move 'Y'                 to ex-ga-direct-deposit
073112     end-if
073112     move co-md-gl-acct          to ex-md-gl-acct
073112     move co-md-div              to ex-md-div
073112     move co-md-center           to ex-md-center
073112     move co-md-amt              to ex-md-amount


072320***  All the new stuff is populated below
072320     move co-stmt-type           to ex-stmt-type
072320     move co-comp-type           to ex-comp-spp
072320     move co-internal-control-1  to ex-internal-control-1
072320     move co-internal-control-2  to ex-internal-control-2
072320
072320     if co-last-maint-hhmmss not numeric
072320        move zeros to co-last-maint-hhmmss
072320     end-if
072320
072320     move co-last-maint-hhmmss   to ws-work-time
072320     string
072320        ws-hh   ':'
072320        ws-mm   ':'
072320        ws-ss delimited by size into ex-last-maint-time
072320     end-string
072320
072320     MOVE CO-roladex-print-dt    TO DC-BIN-DATE-1
072320     MOVE ' '                    TO DC-OPTION-CODE
072320     PERFORM 8510-DATE-CONVERSION
072320                                 THRU 8590-EXIT
072320     IF NO-CONVERSION-ERROR
072320        MOVE DC-GREG-DATE-A-EDIT TO EX-rolodex-print-dt
072320     ELSE
072320        MOVE SPACES              TO EX-rolodex-print-dt
072320     END-IF
072320
072320     move co-ar-bal-level        to ex-ar-balance-level
072320     move co-ar-normal-print     to ex-ar-normal-print
072320     move co-ar-summary-code     to ex-ar-summary-code
072320     move co-ar-reporting        to ex-ar-reporting
072320     move co-ar-pull-check       to ex-ar-pull-check
072320     move co-ar-balance-print    to ex-ar-bal-print
072320     move co-ar-last-run-code    to ex-ar-last-run-code
072320
072320     MOVE CO-LAST-EOM-stmt-dt    TO DC-BIN-DATE-1
072320     MOVE ' '                    TO DC-OPTION-CODE
072320     PERFORM 8510-DATE-CONVERSION
072320                                 THRU 8590-EXIT
072320     IF NO-CONVERSION-ERROR
072320        MOVE DC-GREG-DATE-A-EDIT TO EX-last-eom-stmt-dt
072320     ELSE
072320        MOVE SPACES              TO EX-last-eom-stmt-dt
072320     END-IF
072320
072320     move co-user-code           to ex-user-code
072320     
072320     if co-last-activity-date = zeros or spaces
072320        continue
072320     else
072320        move 20                  to ws-work-century
072320        if co-act-year > 49
072320           move 19               to ws-work-century
072320        end-if
072320        string
072320           co-act-month   '/'
072320           co-act-day     '/'
072320           ws-work-century
072320           co-act-year
072320           delimited by size into ex-last-activity-dt
072320        end-string
072320     end-if
072320
072320     if co-last-stmt-dt = zeros or spaces
072320        continue
072320     else
072320        move 20                  to ws-work-century
072320        if co-last-stmt-year > 49
072320           move 19               to ws-work-century
072320        end-if
072320        string
072320           co-last-stmt-month   '/'
072320           co-last-stmt-day     '/'
072320           ws-work-century
072320           co-last-stmt-year
072320           delimited by size into ex-last-stmt-dt
072320        end-string
072320     end-if
072320
072320     move co-ytd-ovr-undr        to ex-write-off-ytd
072320     move co-cur-fica            to ex-cur-fica
072320     move co-ytd-fica            to ex-ytd-fica
072320     move co-lf-clm-amt          to ex-lf-claim-amt
072320     move co-ah-clm-amt          to ex-ah-claim-amt
072320
072320     if co-current-last-stmt-dt = zeros or spaces
072320        continue
072320     else
072320        move 20                  to ws-work-century
072320        if co-current-last-stmt-year > 49
072320           move 19               to ws-work-century
072320        end-if
072320        string
072320           co-current-last-stmt-month   '/'
072320           co-current-last-stmt-day     '/'
072320           ws-work-century
072320           co-current-last-stmt-year
072320           delimited by size into ex-current-last-stmt-dt
072320        end-string
072320     end-if
072320
072320     MOVE CO-CURRENT-BAL-FWD     TO EX-current-BAL-FWD
072320     MOVE CO-CURRENT-CUR-COM     TO EX-current-CUR-COM
072320     MOVE CO-CURRENT-CUR-CHG     TO EX-current-CUR-CHG
072320     MOVE CO-CURRENT-CUR-PMT     TO EX-current-CUR-PMT
072320     MOVE CO-CURRENT-END-BAL     TO EX-current-END-BAL
072320
072320     MOVE CO-current-CUR         TO EX-AGE-CURRENT-cur
072320     MOVE CO-current-OV30        TO EX-AGE-current-OVER30
072320     MOVE CO-current-OV60        TO EX-AGE-current-OVER60
072320     MOVE CO-current-OV90        TO EX-AGE-current-OVER90
072320     move co-current-ytd-com     to ex-current-ytd-com
072320     move co-current-ytd-ov      to ex-current-ytd-ov
072320     move co-ytd-paid-com        to ex-ytd-paid-comm
072320     move co-ytd-paid-ov         to ex-ytd-paid-ov
072320     move co-current-month-activity
072320                                 to ex-current-month-activity
072320     move co-delinquent-letter-code
072320                                 to ex-delinquent-letter-cd
072320     if dte-client <> 'AHL'
072320        INSPECT co-rptcd2 REPLACING
072320        ALL X'0C'                BY ' '
072320        move co-rptcd2           to ex-report-code-2
072320     end-if
072320     if dte-client = 'AHL'
072320        move co-ov120            to ex-ahl-age-over120
072320        move co-current-ov120    to ex-ahl-age-current-over120
072320     end-if
072320     move co-type-agent          to ex-type-agent
072320     move co-create-ap-check     to ex-create-ap-check
072320     move co-deliver-ck-to-mel   to ex-deliver-to-mel
072320     move co-ach-status          to ex-ach-status
072320
072320     MOVE CO-first-written-dt    TO DC-BIN-DATE-1
072320     MOVE ' '                    TO DC-OPTION-CODE
072320     PERFORM 8510-DATE-CONVERSION
072320                                 THRU 8590-EXIT
072320     IF NO-CONVERSION-ERROR
072320        MOVE DC-GREG-DATE-A-EDIT TO EX-first-written-dt
072320     ELSE
072320        MOVE SPACES              TO EX-first-written-dt
072320     END-IF
072320
072320*    if co-spp-refund-edit <> ' ' AND 'R' AND 'N' AND 'B'
072320*       move spaces              to co-spp-refund-edit
072320*    end-if
072320     if co-spp-refund-edit = X'0C'
072320        move ' ' to co-spp-refund-edit
072320     end-if
072320     move co-spp-refund-edit     to ex-spp-refund-edit

           PERFORM 0080-WRITE-COMP-OUT THRU 0080-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-COMP-OUT.

           INSPECT ERCOMP-DETAIL-RECORD REPLACING
              ALL ';'                  BY ' '
              ALL X'00'                BY ' '
              ALL X'09'                BY ' '

072320     string       
072320        EX-CARRIER                 ';'
072320        EX-GROUPING                ';'
072320        EX-RESP-NO                 ';'
072320        EX-ACCOUNT                 ';'
072320        EX-TYPE                    ';'
072320        EX-STMT-TYPE               ';'
072320        ex-comp-spp                ';'
072320        EX-BALANCE-CONTROL         ';'
072320        EX-INTERNAL-CONTROL-1      ';'
072320        EX-INTERNAL-CONTROL-2      ';'
072320        EX-ACCT-NAME               ';'
072320        EX-MAIL-NAME               ';'
072320        EX-ADDR-1                  ';'
072320        EX-ADDR-2                  ';'
072320        EX-ADDR-3                  ';'
072320        EX-CSO-1099                ';'
072320        EX-ZIP                     ';'
072320        EX-SOC-SEC                 ';'
072320        EX-TELEPHONE               ';'
072320        EX-ROLODEX-PRINT-DT        ';'
072320        EX-AR-BALANCE-LEVEL        ';'
072320        EX-AR-NORMAL-PRINT         ';'
072320        EX-AR-SUMMARY-CODE         ';'
072320        EX-AR-REPORTING            ';'
072320        EX-AR-PULL-CHECK           ';'
072320        EX-AR-BAL-PRINT            ';'
072320        EX-AR-LAST-RUN-CODE        ';'
072320        EX-LAST-EOM-STMT-DT        ';'
072320        EX-USER-CODE               ';'
072320        EX-LAST-ACTIVITY-DT        ';'
072320        EX-LAST-STMT-DT            ';'
072320        EX-BAL-FWD                 ';'
072320        EX-CUR-COM                 ';'
072320        EX-CUR-CHG                 ';'
072320        EX-CUR-PMT                 ';'
072320        EX-END-BAL                 ';'
072320        EX-YTD-COM                 ';'
072320        EX-YTD-OV                  ';'
072320        EX-CSR-CODE                ';'
072320        EX-FAXNO                   ';'
072320        EX-BILL-SW                 ';'
072320        EX-CONTROL-NAME            ';'
072320        EX-STATE                   ';'
072320        EX-STATUS-CODE             ';'
072320        EX-EFF-DT                  ';'
072320        EX-EXP-DT                  ';'
072320        EX-COMMENT-1               ';'
072320        EX-COMMENT-2               ';'
072320        EX-COMMENT-3               ';'
072320        EX-COMMENT-4               ';'
072320        EX-CLP-STATE               ';'
072320        EX-BANK-FEE                ';'
072320        EX-BANK-FEE-LEASE          ';'
072320        EX-LAST-MAINT-USER         ';'
072320        EX-LAST-MAINT-DT           ';'
072320        EX-LAST-MAINT-TIME         ';'
072320        EX-AGE-CURRENT             ';'
072320        EX-AGE-OVER30              ';'
072320        EX-AGE-OVER60              ';'
072320        EX-AGE-OVER90              ';'
072320        EX-STMT-OWNER              ';'
072320        EX-REPORT-GROUP-ID         ';'
072320        ex-ga-withold-pct          ';'
072320        ex-ga-direct-deposit       ';'
072320        ex-md-gl-acct              ';'
072320        ex-md-div                  ';'
072320        ex-md-center               ';'
072320        ex-md-amount               ';'
072320        ex-write-off               ';'
072320        ex-write-off-ytd           ';'
072320        ex-cur-fica                ';'
072320        ex-ytd-fica                ';'
072320        ex-lf-claim-amt            ';'
072320        ex-ah-claim-amt            ';'
072320        EX-current-LAST-STMT-DT    ';'
072320        EX-current-BAL-FWD         ';'
072320        EX-current-CUR-COM         ';'
072320        EX-current-CUR-CHG         ';'
072320        EX-current-CUR-PMT         ';'
072320        EX-current-END-BAL         ';'
072320        EX-AGE-CURRENT-cur         ';'
072320        EX-AGE-current-OVER30      ';'
072320        EX-AGE-current-OVER60      ';'
072320        EX-AGE-current-OVER90      ';'
072320        EX-current-YTD-COM         ';'
072320        EX-current-YTD-OV          ';'
072320        ex-ytd-paid-comm           ';'
072320        ex-ytd-paid-ov             ';'
072320        ex-current-month-activity  ';'
072320        ex-delinquent-letter-cd    ';'
072320        ex-report-code-2           ';'
072320        ex-ahl-age-over120         ';'
072320        ex-ahl-age-current-over120 ';'
072320        ex-type-agent              ';'
072320        ex-create-ap-check         ';'
072320        ex-deliver-to-mel          ';'
072320        ex-ach-status              ';'
072320        ex-first-written-dt        ';'
072320        ex-spp-refund-edit         ';'
072320        EX-EOR
072320        delimited by size into comp-out-rec
072320     end-string

           WRITE COMP-OUT-REC
           ADD 1 TO WS-ERCOMP-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCOMP.

           READ ERCOMP NEXT RECORD

           IF (ERCOMP-FILE-STATUS = '10' OR '23')
              OR (CO-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERCOMP        TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOMP READ ERROR ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERCOMP
              ADD +1                   TO WS-ERCOMP-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERCOMP.

           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD

           START ERCOMP KEY >= CO-CONTROL-PRIMARY

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              SET END-OF-ERCOMP        TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOMP START ERROR ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

