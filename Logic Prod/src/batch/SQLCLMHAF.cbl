      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.                 SQLCLMHAF.
      *                            VMOD=2.001.                          
       AUTHOR.     Cowtown.
       DATE-COMPILED.                                                   
       SECURITY.   *****************************************************
                   *                                                   *
                   *   THIS PROGRAM IS THE PROPERTY OF CSO             *
                   *                                                   *
                   *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
                   *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
                   *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
                   *                                                   *
                   *****************************************************
                                                                        
      *REMARKS.                                                         
      *        THIS PROGRAM LOADS THE CLAIM HIS TABLES

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HISTORY-INPUT-FILE   ASSIGN TO SYS010.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.                                                   
       FILE SECTION.                                                    

       FD  HISTORY-INPUT-FILE          COPY ELCHAF.                     

       FD  DISK-DATE
                                    COPY ELCDTEFD.

       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*  SQLCLMHAF WORKING STORAGE   *'.
       77  FILLER   PIC X(32) VALUE  '******** VMOD=2.001 ************'.

       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-eof-sw                   pic x value spaces.
           88  end-of-input               value 'Y'.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  ws-extract-init             pic x(427) value spaces.


       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(1024).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       ***
      ***     EXEC SQL                                               ***
      ***        insert into ERMEBL (                                ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-birth-dt             pic s9(4) comp value +0.
           05  NU-INC-DT               PIC s9(4) comp value +0.
           05  nu-RPT-DT               pic s9(4) comp value +0.
           05  NU-EST-DT               PIC s9(4) comp value +0.
           05  NU-EST-END-DT           PIC s9(4) comp value +0.
           05  nu-last-pmt-dt          pic s9(4) comp value +0.
           05  nu-pd-thru-dt           pic s9(4) comp value +0.
           05  nu-add-on-dt            pic s9(4) comp value +0.
           05  nu-reopen-dt            pic s9(4) comp value +0.
           05  nu-close-dt             pic s9(4) comp value +0.
           05  nu-purged-dt            pic s9(4) comp value +0.
           05  nu-restored-dt          pic s9(4) comp value +0.
           05  nu-auto-pay-dt          pic s9(4) comp value +0.
           05  nu-resend-dt            pic s9(4) comp value +0.
           05  nu-follow-up-dt         pic s9(4) comp value +0.
           05  nu-maint-dt             pic s9(4) comp value +0.
           05  nu-hist-arch-dt         pic s9(4) comp value +0.
           05  nu-act-maint-dt         pic s9(4) comp value +0.
           05  nu-ben-exp-dt           pic s9(4) comp value +0.

       01  CLAIM-MASTER-extract.
           05  ex-CARRIER              PIC X.
           05  ex-CLAIM-NO             PIC X(7).
           05  ex-CERT-NO              pic x(11).
           05  ex-INSURED-LAST-NAME    PIC X(15).
           05  ex-INSURED-1ST-NAME     PIC X(12).
           05  ex-INSURED-MID-INIT     PIC X.
           05  ex-INSURED-BIRTH-DT     PIC X(10).
           05  ex-INSURED-SEX-CD       PIC X.
           05  ex-INSURED-OCC-CD       PIC X(6).
           05  ex-PROCESSOR-ID         PIC X(4).
           05  ex-CLAIM-STATUS         PIC X.
           05  ex-CLAIM-TYPE           PIC X.
           05  ex-CLAIM-PREM-TYPE      PIC X.
           05  ex-INCURRED-DT          PIC X(10).
           05  ex-REPORTED-DT          PIC X(10).
           05  ex-FILE-ESTABLISH-DT    PIC X(10).
           05  ex-EST-END-OF-DISAB-DT  PIC X(10).
           05  ex-LAST-PMT-DT          PIC X(10).
           05  ex-LAST-PMT-AMT-n       PIC -9(7).99.
           05  ex-last-pmt-amt redefines
               ex-last-pmt-amt-n       pic x(11).
           05  ex-PAID-THRU-DT         PIC X(10).
           05  ex-TOTAL-PAID-AMT-n     PIC -9(7).99.
           05  ex-total-paid-amt redefines
               ex-total-paid-amt-n     pic x(11).
           05  ex-NO-OF-PMTS-MADE      PIC 999.
           05  ex-NO-OF-DAYS-PAID      PIC 9(05).
           05  ex-PMT-CALC-METHOD      PIC X.
           05  ex-CAUSE-CD             PIC X(6).
           05  ex-PRIME-CERT-NO        pic x(11).
           05  ex-BENEFIT-PERIOD       PIC 99.
           05  ex-PROG-FORM-TYPE       PIC X.
           05  ex-LAST-ADD-ON-DT       PIC X(10).
           05  ex-LAST-REOPEN-DT       PIC X(10).
           05  ex-LAST-CLOSE-DT        PIC X(10).
           05  ex-LAST-CLOSE-REASON    PIC X(01).
           05  ex-CLAIM-PAYMENT-STATUS PIC 9.
           05  ex-TOTAL-INT-PAID-n     PIC -9(5).99.
           05  ex-total-int-paid redefines
               ex-total-int-paid-n     pic x(9).
           05  ex-CERT-ORIGIN          PIC X.
           05  ex-CERT-STATE           PIC XX.
           05  ex-CERT-ACCOUNT         pic x(10).
           05  ex-CERT-EFF-DT          PIC X(10).
           05  ex-PRIORITY-CD          PIC X.
           05  ex-SUPV-ATTN-CD         PIC X.
           05  ex-PURGED-DT            PIC X(10).
           05  ex-RESTORED-DT          PIC X(10).
           05  ex-NEXT-AUTO-PAY-DT     PIC X(10).
           05  ex-NEXT-RESEND-DT       PIC X(10).
           05  ex-NEXT-FOLLOWUP-DT     PIC X(10).
           05  ex-CRITICAL-PERIOD      PIC 99.   *>  00
           05  ex-LAST-MAINT-DT        PIC X(10).
           05  ex-LAST-MAINT-USER      PIC X(4).
           05  ex-LAST-MAINT-HHMMSS    PIC 9(6).
           05  ex-LAST-MAINT-TYPE      PIC X.
           05  ex-RELATED-CLAIM-NO     PIC X(7).
           05  ex-HISTORY-ARCHIVE-DT   PIC X(10).
           05  ex-BENEFICIARY          PIC X(10).
           05  ex-FILE-ESTABLISHED-BY  PIC X(4).
           05  ex-DENIAL-TYPE          PIC X.
           05  ex-NO-OF-EXTENSIONS     PIC 99.   *>  00
           05  ex-TRAILER-SEQ-CNT      PIC 9(5).
           05  ex-LAST-INC-DT-CHANGE   PIC 9(5).
           05  ex-AUTO-PAY-SEQ         PIC 9(5).
           05  ex-INSURED-ADDR-CNT     PIC 9(1).
           05  ex-ACCOUNT-ADDR-CNT     PIC 9(1).
           05  ex-BENIF-ADDR-CNT       PIC 9(1).
           05  ex-EMPLOYER-ADDR-CNT    PIC 9(1).
           05  ex-DOCTOR-ADDR-CNT      PIC 9(1).
           05  ex-OTHER-1-ADDR-CNT     PIC 9(1).
           05  ex-OTHER-2-ADDR-CNT     PIC 9(1).
           05  ex-FILE-LOCATION        PIC X(4).
           05  ex-FATAL-ERROR-CNT      PIC 9(5).
           05  ex-FORCEABLE-ERROR-CNT  PIC 9(5).
           05  ex-PRODUCT-CD           PIC X.
           05  ex-ACTIVITY-CODE        pic 99.
           05  ex-ACTIVITY-MAINT-DT    PIC X(10).
           05  ex-ACTIVITY-MAINT-TYPE  PIC X(4).
           05  ex-LAPSE-REPORT-CODE    PIC 9.
           05  ex-LAG-REPORT-CODE      PIC 9.
           05  ex-LOAN-TYPE            PIC XX.
           05  ex-LEGAL-STATE          PIC XX.
           05  ex-YESNOSW              PIC X.
           05  ex-ACCIDENT-CLAIM-SW    PIC X.
           05  ex-insured-type         pic x.
           05  ex-benefit-exp-dt       PIC X(10).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  FILLER.                                                      
           05  CLAIM-IN-CNT                PIC S9(07) COMP-3 VALUE +0.  
           05  CLAIM-OUT-CNT               PIC S9(07) COMP-3 VALUE +0.  
           05  HISTORY-IN-CNT              PIC S9(07) COMP-3 VALUE +0.  
           05  WS-ZERO                     PIC S9 COMP-3 VALUE +0.      
           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.  
           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE SPACES.  
           05  WS-RETURN-CODE              PIC S9(04) COMP VALUE +0.    
           05  PGM-SUB                 PIC S999  VALUE +344 COMP-3.

                                       COPY ELCMSTR.
                                       COPY ELCDTECX.
                                       copy ELCDTEVR.
                                       COPY ELCDATE.
                                                                        
      ******************************************************************
       LINKAGE SECTION.                                                 
       01  parm.
           05  parm-length             pic s9(4) comp.
           05  parm-current-month-end  pic 9(8).
           05  parm-program-option     pic x.

       01  var  pic x(30).

       procedure division using parm.
                                       COPY ELCDTERX.
                                                                        
       0000-begin.

           display ' Begin Program SQLCLMHS '
           perform 0010-open-files     thru 0010-exit
           perform 0020-init           thru 0020-exit
           perform 2000-connect-to-logic
                                       thru 2000-exit
           perform 1000-truncate-table thru 1000-exit
           perform 0030-read-input     thru 0030-exit
           perform 0100-process-input  thru 0100-exit until
              end-of-input  *> or claim-out-cnt > +1000
           perform 2500-finish-up      thru 2500-exit
           goback

           .
       0010-open-files.

           OPEN INPUT HISTORY-INPUT-FILE

           .
       0010-exit.
           exit.

       0020-init.

           move spaces                 to claim-master-extract

           move zeros                  to ex-last-pmt-amt-n
                                          ex-total-paid-amt-n
                                          ex-no-of-pmts-made
                                          ex-no-of-days-paid
                                          ex-total-int-paid-n
                                          ex-critical-period
                                          ex-last-maint-hhmmss
                                          ex-no-of-extensions
                                          ex-trailer-seq-cnt
                                          ex-last-inc-dt-change
                                          ex-auto-pay-seq
                                          ex-insured-addr-cnt
                                          ex-account-addr-cnt
                                          ex-benif-addr-cnt
                                          ex-employer-addr-cnt
                                          ex-doctor-addr-cnt
                                          ex-other-1-addr-cnt
                                          ex-other-2-addr-cnt
                                          ex-fatal-error-cnt
                                          ex-forceable-error-cnt

           move claim-master-extract      to ws-extract-init

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS  ' ws-kix-myenv

           EXEC SQL
              SET AUTOCOMMIT OFF
           END-EXEC

           .
       0020-exit.
           exit.
       0030-read-input.

           READ HISTORY-INPUT-FILE AT END                               
              set end-of-input to true
           end-read

           if not end-of-input
              ADD +1 TO HISTORY-IN-CNT
              IF (HIR-RECORD-ID = 'CL')
                 and (hir-company-id = dte-client)
                 ADD +1  TO CLAIM-IN-CNT
                 MOVE HIR-CLAIM-RECORD TO CLAIM-MASTER
              else
                 GO TO 0030-read-input
              end-if
           end-if

           .
       0030-exit.
           exit.

       0100-process-input.

           move ws-extract-init          to claim-master-extract

           if cl-last-pmt-amt not numeric
              move zeros               to cl-last-pmt-amt
           end-if
           if cl-total-paid-amt not numeric
              move zeros               to cl-total-paid-amt
           end-if
           if cl-no-of-pmts-made not numeric
              move zeros               to cl-no-of-pmts-made
           end-if
           if cl-no-of-days-paid not numeric
              move zeros               to cl-no-of-days-paid
           end-if
           if cl-benefit-period not numeric
              move zeros               to cl-benefit-period
           end-if
           if cl-claim-payment-status not numeric
              move zeros               to cl-claim-payment-status
           end-if
           if cl-total-int-paid not numeric
              move zeros               to cl-total-int-paid
           end-if
           if cl-last-maint-hhmmss not numeric
              move zeros               to cl-last-maint-hhmmss
           end-if
           if cl-no-of-extensions not numeric
              move zeros               to cl-no-of-extensions
           end-if
           if cl-trailer-seq-cnt not numeric
              move zeros               to cl-trailer-seq-cnt
           end-if
           if CL-LAST-INC-DT-CHANGE not numeric
              move zeros               to CL-LAST-INC-DT-CHANGE
           end-if
           if cl-auto-pay-seq not numeric
              move zeros               to cl-auto-pay-seq
           end-if
           if cl-insured-addr-cnt not numeric
              move zeros               to cl-insured-addr-cnt
           end-if
           if cl-account-addr-cnt not numeric
              move zeros               to cl-account-addr-cnt
           end-if
           if cl-benif-addr-cnt not numeric
              move zeros               to cl-benif-addr-cnt
           end-if
           if cl-employer-addr-cnt not numeric
              move zeros               to cl-employer-addr-cnt
           end-if
           if cl-doctor-addr-cnt not numeric
              move zeros               to cl-doctor-addr-cnt
           end-if
           if cl-other-1-addr-cnt not numeric
              move zeros               to cl-other-1-addr-cnt
           end-if
           if cl-other-2-addr-cnt not numeric
              move zeros               to cl-other-2-addr-cnt
           end-if
           if cl-fatal-error-cnt not numeric
              move zeros               to cl-fatal-error-cnt
           end-if
           if cl-forceable-error-cnt not numeric
              move zeros               to cl-forceable-error-cnt
           end-if
           if cl-activity-code not numeric
              move zeros               to cl-activity-code
           end-if
           if cl-lapse-report-code not numeric
              move zeros               to cl-lapse-report-code
           end-if
           if cl-lag-report-code not numeric
              move zeros               to cl-lag-report-code
           end-if
           if cl-yesnosw = low-values
              move spaces               to cl-yesnosw
           end-if
           if cl-critical-period not numeric
              move zeros                 to cl-critical-period
           end-if

           move cl-carrier               to ex-CARRIER              
           move cl-claim-no              to ex-CLAIM-NO             
           move cl-cert-no               to ex-CERT-NO              
           move cl-insured-last-name     to ex-INSURED-LAST-NAME    
           move cl-insured-1st-name      to ex-INSURED-1ST-NAME     
           move cl-insured-mid-init      to ex-INSURED-MID-INIT     

           if cl-insured-birth-dt = spaces or low-values
              continue
           else
              move cl-insured-birth-dt   to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert  thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit to ex-insured-birth-dt
              end-if
           end-if

           move cl-insured-sex-cd        to ex-INSURED-SEX-CD       
           move cl-insured-occ-cd        to ex-INSURED-OCC-CD       
           move cl-processor-id          to ex-PROCESSOR-ID         
           move cl-claim-status          to ex-CLAIM-STATUS         
           move cl-claim-type            to ex-CLAIM-TYPE           
           move cl-claim-prem-type       to ex-CLAIM-PREM-TYPE      

           if cl-incurred-dt = spaces or low-values
              continue
           else
              move cl-incurred-dt           to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-INCURRED-DT
              end-if
           end-if

           if cl-reported-dt = spaces or low-values
              continue
           else
              move cl-reported-dt           to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-REPORTED-DT
              end-if
           end-if
              
           if cl-file-establish-dt = spaces or low-values
              continue
           else
              move cl-file-establish-dt     to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-FILE-ESTABLISH-DT
              end-if
           end-if
              
           if cl-est-end-of-disab-dt = spaces or low-values
              continue
           else
              move cl-est-end-of-disab-dt   to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-EST-END-OF-DISAB-DT
              end-if
           end-if
              
           if cl-last-pmt-dt = spaces or low-values
              continue
           else
              move cl-last-pmt-dt           to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-LAST-PMT-DT
              end-if
           end-if

           move cl-last-pmt-amt          to ex-LAST-PMT-AMT-n       

           if cl-paid-thru-dt = spaces or low-values
              continue
           else
              move cl-paid-thru-dt          to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-PAID-THRU-DT
              end-if
           end-if

           move cl-total-paid-amt        to ex-TOTAL-PAID-AMT-n     
           move cl-no-of-pmts-made       to ex-NO-OF-PMTS-MADE      
           move cl-no-of-days-paid       to ex-NO-OF-DAYS-PAID      
           move cl-pmt-calc-method       to ex-PMT-CALC-METHOD      
           move cl-cause-cd              to ex-CAUSE-CD             
           move cl-prime-cert-no         to ex-PRIME-CERT-NO        
           move cl-benefit-period        to ex-BENEFIT-PERIOD       
           move cl-prog-form-type        to ex-PROG-FORM-TYPE       

           if cl-last-add-on-dt = spaces or low-values
              continue
           else
              move cl-last-add-on-dt        to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-LAST-ADD-ON-DT
              end-if
           end-if
              
           if cl-last-reopen-dt = spaces or low-values
              continue
           else
              move cl-last-reopen-dt        to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-LAST-REOPEN-DT
              end-if
           end-if
              
           if cl-last-close-dt = spaces or low-values
              continue
           else
              move cl-last-close-dt         to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-LAST-CLOSE-DT
              end-if
           end-if

           move cl-last-close-reason     to ex-LAST-CLOSE-REASON    
           move cl-claim-payment-status  to ex-CLAIM-PAYMENT-STATUS 
           move cl-total-int-paid        to ex-TOTAL-INT-PAID-n     
           move cl-cert-origin           to ex-CERT-ORIGIN          
           move cl-cert-state            to ex-CERT-STATE           
           move cl-cert-account          to ex-CERT-ACCOUNT         

           if cl-cert-eff-dt = spaces or low-values
              continue
           else
              move cl-cert-eff-dt           to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-CERT-EFF-DT
              end-if
           end-if

           move cl-priority-cd           to ex-PRIORITY-CD          
           move cl-supv-attn-cd          to ex-SUPV-ATTN-CD         

           if cl-purged-dt = spaces or low-values
              continue
           else
              move cl-purged-dt             to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-PURGED-DT
              end-if
           end-if

           if cl-restored-dt = spaces or low-values
              continue
           else
              move cl-restored-dt           to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-RESTORED-DT
              end-if
           end-if
              
           if cl-next-auto-pay-dt = spaces or low-values
              continue
           else
              move cl-next-auto-pay-dt      to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-NEXT-AUTO-PAY-DT
              end-if
           end-if
              
           if cl-next-resend-dt = spaces or low-values
              continue
           else
              move cl-next-resend-dt        to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-NEXT-RESEND-DT
              end-if
           end-if
              
           if cl-next-followup-dt = spaces or low-values
              continue
           else
              move cl-next-followup-dt      to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-NEXT-FOLLOWUP-DT
              end-if
           end-if

           move cl-critical-period       to ex-CRITICAL-PERIOD      

           if cl-last-maint-dt = spaces or low-values
              continue
           else
              move cl-last-maint-dt         to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-LAST-MAINT-DT
              end-if
           end-if

           move cl-last-maint-user       to ex-LAST-MAINT-USER      
           move cl-last-maint-hhmmss     to ex-LAST-MAINT-HHMMSS    
           move cl-last-maint-type       to ex-LAST-MAINT-TYPE      
           move cl-related-claim-no      to ex-RELATED-CLAIM-NO     

           if cl-history-archive-dt = spaces or low-values
              continue
           else
              move cl-history-archive-dt    to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-HISTORY-ARCHIVE-DT
              end-if
           end-if

           move cl-beneficiary           to ex-BENEFICIARY          
           move cl-file-established-by   to ex-FILE-ESTABLISHED-BY  
           move cl-denial-type           to ex-DENIAL-TYPE          
           move cl-no-of-extensions      to ex-NO-OF-EXTENSIONS     
           move cl-trailer-seq-cnt       to ex-TRAILER-SEQ-CNT      
           move cl-last-inc-dt-change    to ex-LAST-INC-DT-CHANGE   
           move cl-auto-pay-seq          to ex-AUTO-PAY-SEQ         
           move cl-insured-addr-cnt      to ex-INSURED-ADDR-CNT     
           move cl-account-addr-cnt      to ex-ACCOUNT-ADDR-CNT     
           move cl-benif-addr-cnt        to ex-BENIF-ADDR-CNT       
           move cl-employer-addr-cnt     to ex-EMPLOYER-ADDR-CNT    
           move cl-doctor-addr-cnt       to ex-DOCTOR-ADDR-CNT      
           move cl-other-1-addr-cnt      to ex-OTHER-1-ADDR-CNT     
           move cl-other-2-addr-cnt      to ex-OTHER-2-ADDR-CNT     
           move cl-file-location         to ex-FILE-LOCATION        
           move cl-fatal-error-cnt       to ex-FATAL-ERROR-CNT      
           move cl-forceable-error-cnt   to ex-FORCEABLE-ERROR-CNT  
           move cl-product-cd            to ex-PRODUCT-CD           
           move cl-activity-code         to ex-ACTIVITY-CODE        

           if cl-activity-maint-dt = spaces or low-values
              continue
           else
              move cl-activity-maint-dt     to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-ACTIVITY-MAINT-DT
              end-if
           end-if

           move cl-activity-maint-type   to ex-ACTIVITY-MAINT-TYPE  
           move cl-lapse-report-code     to ex-LAPSE-REPORT-CODE    
           move cl-lag-report-code       to ex-LAG-REPORT-CODE      
           move cl-loan-type             to ex-LOAN-TYPE            
           move cl-legal-state           to ex-LEGAL-STATE          
           move cl-yesnosw               to ex-YESNOSW              
           move cl-accident-claim-sw     to ex-ACCIDENT-CLAIM-SW    
           move cl-insured-type          to ex-insured-type         

           if cl-benefit-expiration-dt = spaces or low-values
              continue
           else
              move cl-benefit-expiration-dt to dc-bin-date-1
              set bin-to-greg to true
              perform 8500-date-convert     thru 8500-exit
              if no-conversion-error
                 move dc-greg-date-a-edit   to ex-benefit-exp-dt
              end-if
           end-if

           perform 0110-insert-extract thru 0110-exit
           perform 0030-read-input     thru 0030-exit

           .
       0100-exit.
           exit.

       0110-insert-extract.

           perform 0120-set-nulls      thru 0120-exit
           if ex-claim-no = '0187566'
              display ' found claim 0187566 '
              display ' birth     *' ex-insured-birth-dt '*'
                 nu-birth-dt
              move '09/30/1907' to ex-insured-birth-dt
           end-if

           EXEC SQL
              insert into ELMSTR_HAF (
                  Carrier
                 ,ClaimNo
                 ,CertNo
                 ,LastName
                 ,FirstName
                 ,MidInit
                 ,BirthDate
                 ,Gender
                 ,Occupation
                 ,ProcId
                 ,ClmStatus
                 ,ClmType
                 ,PremType
                 ,IncurredDate
                 ,ReportedDate
                 ,EstablishDate
                 ,EstEndOfDiDate
                 ,LastPmtDate
                 ,LastPmtAmt
                 ,PaidThruDate
                 ,TotalPaid
                 ,NoOfPmts
                 ,NoOfDaysPaid
                 ,PmtCalcMethod
                 ,CauseCode
                 ,PrimeCertNo
                 ,BenefitPeriod
                 ,ProgFormType
                 ,LastAddOnDate
                 ,LastReopenDate
                 ,LastCloseDate
                 ,LastCloseReason
                 ,PmtStatus
                 ,TotInterestPaid
                 ,CertOrigin
                 ,CertState
                 ,CertAccount
                 ,CertEffDate
                 ,PriorityCd
                 ,SupvAttnCd
                 ,PurgedDate
                 ,RestoredDate
                 ,NextAutoPayDate
                 ,NextResendDate
                 ,NextFollowupDate
                 ,CritiPeriod
                 ,LastMaintDate
                 ,LastMaintUser
                 ,LastMaintTime
                 ,LastMaintType
                 ,RelClaimNo
                 ,HisArchiveDate
                 ,Beneficiary
                 ,EstablishUser
                 ,DenialType
                 ,NoOfExtensions
                 ,TrlrSeqCntr
                 ,LastIncDtChg
                 ,AutoPaySeq
                 ,InsAddrCnt
                 ,AcctAddrCnt
                 ,BeneAddrCnt
                 ,EmpAddrCnt
                 ,DocAddrCnt
                 ,Oth1AddrCnt
                 ,Oth2AddrCnt
                 ,FileLocal
                 ,FatalErrorCnt
                 ,ForceErrorCnt
                 ,ProductCd
                 ,ActivityCd
                 ,ActMaintDate
                 ,ActMaintType
                 ,LapseReportCd
                 ,LagReportCd
                 ,LoanType
                 ,LegalState
                 ,YesNo
                 ,AccClaimSw
                 ,InsuredType
                 ,BenefitExpDate) 
	            values (
                  :ex-CARRIER            
                 ,:ex-CLAIM-NO           
                 ,:ex-CERT-NO            
                 ,:ex-INSURED-LAST-NAME  
                 ,:ex-INSURED-1ST-NAME   
                 ,:ex-INSURED-MID-INIT   
                 ,:ex-INSURED-BIRTH-DT  :nu-birth-dt
                 ,:ex-INSURED-SEX-CD     
                 ,:ex-INSURED-OCC-CD     
                 ,:ex-PROCESSOR-ID       
                 ,:ex-CLAIM-STATUS       
                 ,:ex-CLAIM-TYPE         
                 ,:ex-CLAIM-PREM-TYPE    
                 ,:ex-INCURRED-DT  :nu-inc-dt
                 ,:ex-REPORTED-DT  :nu-rpt-dt
                 ,:ex-FILE-ESTABLISH-DT  :nu-est-dt
                 ,:ex-EST-END-OF-DISAB-DT  :nu-est-end-dt
                 ,:ex-LAST-PMT-DT  :nu-last-pmt-dt
                 ,:ex-LAST-PMT-AMT
                 ,:ex-PAID-THRU-DT  :nu-pd-thru-dt
                 ,:ex-TOTAL-PAID-AMT
                 ,:ex-NO-OF-PMTS-MADE     
                 ,:ex-NO-OF-DAYS-PAID     
                 ,:ex-PMT-CALC-METHOD     
                 ,:ex-CAUSE-CD            
                 ,:ex-PRIME-CERT-NO       
                 ,:ex-BENEFIT-PERIOD      
                 ,:ex-PROG-FORM-TYPE      
                 ,:ex-LAST-ADD-ON-DT  :nu-add-on-dt
                 ,:ex-LAST-REOPEN-DT  :nu-reopen-dt
                 ,:ex-LAST-CLOSE-DT  :nu-close-dt
                 ,:ex-LAST-CLOSE-REASON   
                 ,:ex-CLAIM-PAYMENT-STATUS
                 ,:ex-TOTAL-INT-PAID
                 ,:ex-CERT-ORIGIN        
                 ,:ex-CERT-STATE         
                 ,:ex-CERT-ACCOUNT       
                 ,:ex-CERT-EFF-DT        
                 ,:ex-PRIORITY-CD        
                 ,:ex-SUPV-ATTN-CD       
                 ,:ex-PURGED-DT  :nu-purged-dt
                 ,:ex-RESTORED-DT  :nu-restored-dt
                 ,:ex-NEXT-AUTO-PAY-DT  :nu-auto-pay-dt
                 ,:ex-NEXT-RESEND-DT  :nu-resend-dt
                 ,:ex-NEXT-FOLLOWUP-DT  :nu-follow-up-dt
                 ,:ex-CRITICAL-PERIOD    
                 ,:ex-LAST-MAINT-DT  :nu-maint-dt
                 ,:ex-LAST-MAINT-USER    
                 ,:ex-LAST-MAINT-HHMMSS  
                 ,:ex-LAST-MAINT-TYPE    
                 ,:ex-RELATED-CLAIM-NO   
                 ,:ex-HISTORY-ARCHIVE-DT  :nu-hist-arch-dt
                 ,:ex-BENEFICIARY        
                 ,:ex-FILE-ESTABLISHED-BY
                 ,:ex-DENIAL-TYPE        
                 ,:ex-NO-OF-EXTENSIONS   
                 ,:ex-TRAILER-SEQ-CNT    
                 ,:ex-LAST-INC-DT-CHANGE 
                 ,:ex-AUTO-PAY-SEQ       
                 ,:ex-INSURED-ADDR-CNT   
                 ,:ex-ACCOUNT-ADDR-CNT   
                 ,:ex-BENIF-ADDR-CNT     
                 ,:ex-EMPLOYER-ADDR-CNT  
                 ,:ex-DOCTOR-ADDR-CNT    
                 ,:ex-OTHER-1-ADDR-CNT   
                 ,:ex-OTHER-2-ADDR-CNT   
                 ,:ex-FILE-LOCATION      
                 ,:ex-FATAL-ERROR-CNT    
                 ,:ex-FORCEABLE-ERROR-CNT
                 ,:ex-PRODUCT-CD         
                 ,:ex-ACTIVITY-CODE      
                 ,:ex-ACTIVITY-MAINT-DT  :nu-act-maint-dt
                 ,:ex-ACTIVITY-MAINT-TYPE
                 ,:ex-LAPSE-REPORT-CODE  
                 ,:ex-LAG-REPORT-CODE    
                 ,:ex-LOAN-TYPE          
                 ,:ex-LEGAL-STATE        
                 ,:ex-YESNOSW            
                 ,:ex-ACCIDENT-CLAIM-SW  
                 ,:ex-insured-type       
                 ,:ex-benefit-exp-dt  :nu-ben-exp-dt)
           end-exec

           if sqlcode not = 0
              display "Error: cannot insert row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' claim-in-cnt ' ' claim-out-cnt
              display ' offending rec ' CLAIM-MASTER-extract
           else
              add 1 to claim-out-cnt
           end-if

           .
       0110-exit.
           exit.

       0120-set-nulls.

           move +0                     to nu-birth-dt    
                                          NU-INC-DT      
                                          nu-RPT-DT      
                                          NU-EST-DT      
                                          NU-EST-END-DT  
                                          nu-last-pmt-dt 
                                          nu-pd-thru-dt  
                                          nu-add-on-dt   
                                          nu-reopen-dt   
                                          nu-close-dt    
                                          nu-restored-dt 
                                          nu-auto-pay-dt 
                                          nu-resend-dt   
                                          nu-follow-up-dt
                                          nu-maint-dt    
                                          nu-hist-arch-dt
                                          nu-purged-dt   
                                          nu-act-maint-dt
                                          nu-ben-exp-dt  

           if ex-insured-birth-dt = spaces
              move -1                  to nu-birth-dt
           end-if
           if ex-incurred-dt = spaces
              move -1                  to nu-inc-dt
           end-if
           if ex-reported-dt = spaces
              move -1                  to nu-rpt-dt
           end-if
           if ex-file-establish-dt = spaces
              move -1                  to nu-est-dt
           end-if
           if ex-est-end-of-disab-dt = spaces
              move -1                  to nu-est-end-dt
           end-if
           if ex-last-pmt-dt = spaces
              move -1                  to nu-last-pmt-dt
           end-if
           if ex-paid-thru-dt = spaces
              move -1                  to nu-pd-thru-dt
           end-if
           if ex-last-add-on-dt = spaces
              move -1                  to nu-add-on-dt
           end-if
           if ex-last-reopen-dt = spaces
              move -1                  to nu-reopen-dt
           end-if
           if ex-last-close-dt = spaces
              move -1                  to nu-close-dt
           end-if
           if ex-purged-dt = spaces
              move -1                  to nu-purged-dt
           end-if
           if ex-restored-dt = spaces
              move -1                  to nu-restored-dt
           end-if
           if ex-next-auto-pay-dt = spaces
              move -1                  to nu-auto-pay-dt
           end-if
           if ex-next-resend-dt = spaces
              move -1                  to nu-resend-dt
           end-if
           if ex-next-followup-dt = spaces
              move -1                  to nu-follow-up-dt
           end-if
           if ex-last-maint-dt = spaces
              move -1                  to nu-maint-dt
           end-if
           if ex-history-archive-dt = spaces
              move -1                  to nu-hist-arch-dt
           end-if
           if ex-activity-maint-dt = spaces
              move -1                  to nu-act-maint-dt
           end-if
           if ex-benefit-exp-dt = spaces
              move -1                  to nu-ben-exp-dt
           end-if

           .
       0120-exit.
           exit.

       1000-truncate-table.

           display 'Begin Truncate table'
           EXEC SQL
               truncate table ELMSTR_HAF
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       1000-exit.
           exit.

       2000-connect-to-logic.

           display ' about to connect to Logic '

063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

      *    EXEC SQL
      *       SET OPTION logintime 5
      *    END-EXEC

           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to PEMA"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       2000-exit.
           exit.

       2500-finish-up.

           CLOSE HISTORY-INPUT-FILE                                     
                                                                        
           DISPLAY '*******************************************'.       
           DISPLAY ' HISTORY IN     ...... ' HISTORY-IN-CNT             
           DISPLAY ' CLAIMS  IN     ...... ' CLAIM-IN-CNT               
           DISPLAY ' CLAIMS  OUT    ...... ' CLAIM-OUT-CNT              
           DISPLAY '*******************************************'.       

           EXEC SQL
               commit work release
           END-EXEC

           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       2500-exit.
           exit.

       8500-date-convert.

           call 'ELDATCX' using date-conversion-data

           .
       8500-exit.
           exit.

       abend-pgm.

           call 'ABORTME'

           goback.
