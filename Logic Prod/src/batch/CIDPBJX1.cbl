       IDENTIFICATION DIVISION.
                                                                        
       PROGRAM-ID. CIDPBJX.
      *AUTHOR.     CSO.
      *            Omaha, NE.
      *DATE-COMPILED.                                                   
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS. THIS PROGRAM Creates extracts of the journal records
      * logged by the ERPNDB 	FILE.
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT PB-JOURNAL-IN    ASSIGN TO SYS010.
           SELECT FILE-OUT         ASSIGN TO SYS011
              organization line sequential.
           SELECT DISK-DATE        ASSIGN TO SYS019.
     
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  PB-JOURNAL-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  RT-JOURNAL-IN-REC.
           05  filler                  pic x(34).
           05  pb-jp-rest-of-rec.
               10  filler              pic x(30).
               10  pb-jp-erpndb        pic x(585).

       FD  FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  FILE-OUT-REC                PIC X(100).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.                   

       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    CIDPBJX  WORKING-STORAGE    '.  
       77  FILLER  PIC X(32) VALUE '*****VMOD=2.001*****************'.  
                                                                        
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-recs-out                 pic 9(7) value zeros.
       77  ws-recs-byp                 pic 9(7) value zeros.
       77  ws-input-sw                 pic x value spaces.
           88  end-of-input                 value 'Y'.


       01  ws-extract-out              pic x(100) value spaces.
       01  ws-hold-rec-ind             pic x   value ' '.
           88  holding-record             value 'Y'.
       01  ws-held-record              pic x(100) value low-values.
       01  JOURNAL-RECORD.
           12  jp-date                 pic s9(5) comp-3.
           12  jp-time                 pic s9(7) comp-3.
           12  JP-USER-ID              PIC X(4).
           12  JP-FILE-ID              PIC X(8).
           12  JP-PROGRAM-ID           PIC X(8).
           12  JP-RECORD-TYPE          PIC X.
               88 JP-ADD                  VALUE 'A'.
               88 JP-BEFORE-CHANGE        VALUE 'B'.
               88 JP-AFTER-CHANGE         VALUE 'C'.
               88 JP-DELETE               VALUE 'D'.
               88 JP-GENERIC-DELETE       VALUE 'G'.
               88 JP-KEY-CHG-DELETE       VALUE 'K'.
               88 JP-KEY-CHG-ADD          VALUE 'N'.
           12  JP-GENERIC-KEY-LENGTH   PIC S9(4)   COMP.
           12  JP-RECORD-AREA          pic x(585).

       01  ws-previous-stuff.
           05  ps-date                 pic s9(5) comp-3 value +0.
           05  ps-user-id              pic x(04)  value spaces.
           05  ps-file-id              pic x(08)  value spaces.
           05  ps-program-id           pic x(08)  value spaces.
           05  ps-record-type          pic x      value spaces.
           05  ps-record-body          pic x(375) value spaces.

       01  WS-ABEND.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
           12  WS-ZERO                 PIC S9          VALUE ZERO.      
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
                                                                        
       01  filler.
           05  sort-key.
               10  sk-date             pic x(10).
       10  sk-time             pic x(8).

       01  WS.
           12  ws-work-time            pic x(7).
           12  ws-work-time-n redefines ws-work-time
                                       pic 9(7).
                                       
           12  PGM-SUB                 PIC S999    COMP    VALUE +072.  

                                       COPY ERCPNDB.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       01  ws-date.
           05  ws-audit-report-dt      pic 9(8) value zeros.

       01  ws-julian-rpt-dt            pic 9(5) value zeros.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           perform 0000-open-files     thru 0000-exit
           perform 0010-init           thru 0010-exit
           perform 0110-process-input  thru 0110-exit until
              end-of-input

           go to 0510-EOJ-CLOSER

           goback

           .                                                                        
       0000-OPEN-FILES.                                                 
                                                                        
           OPEN INPUT PB-JOURNAL-IN
               OUTPUT FILE-OUT
           .
       0000-exit.
           exit.

       0010-init.

           move zeros                  to ws-date
                                          ws-julian-rpt-dt


           if ws-date not = zeros
              move ws-audit-report-dt  to dc-greg-date-cymd
              move 'L'                 to dc-option-code
              perform 8500-date-convert thru 8500-exit
              if no-conversion-error
                 display ' julian date ***' dc-julian-date '***'
                 move dc-julian-date   to ws-julian-rpt-dt
                 display ' Report date override ' ws-julian-rpt-dt
                    ' ' ws-date (5:2) '/' ws-date (7:2) '/'
                    ws-date (1:4)
              else
                 display ' bad date convert ' dc-error-code
                 move zeros            to ws-julian-rpt-dt
              end-if
           end-if

           perform 0120-read-input     thru 0120-exit
           perform 0020-build-previous thru 0020-exit

           .
       0010-exit.
           exit.

       0020-build-previous.

           move jp-date                to ps-date
           move jp-user-id             to ps-user-id
           move jp-file-id             to ps-file-id
           move jp-program-id          to ps-program-id
           move jp-record-type         to ps-record-type
           move pb-record-body         to ps-record-body

           .
       0020-exit.
           exit.

       0110-process-input.

           if pb-company-cd not = dte-clasic-company-cd
              go to 0110-continue
           end-if

      *    if jp-record-type <> 'A' AND 'C' AND 'D'
      *       display ' bypassing type ' jp-record-type
      *       go to 0110-continue
      *    end-if

      *    if pb-record-type <> '1' and '2'
      *       display ' bypass pending rec type ' pb-record-type
      *       go to 0110-continue
      *    end-if           

           move jp-date                to dc-julian-date
           move '5'                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit   to sk-date
           else
              move zeros               to sk-date
           end-if
           move jp-time                to ws-work-time-n

           string
              ws-work-time(2:2)   ':'
              ws-work-time(4:2)   ':'
              ws-work-time(6:2) delimited by size
                 into sk-time
           end-string

           perform 0130-build-record   thru 0130-exit

           .
       0110-continue.

           perform 0120-read-input     thru 0120-exit

           .
       0110-exit.
           exit.

       0120-READ-input.
                                                                        
           READ PB-JOURNAL-IN AT END
              set end-of-input         to true
           END-READ
     
           if not end-of-input
              MOVE pb-jp-rest-of-rec   TO JOURNAL-RECORD
              MOVE jp-RECORD-AREA      TO pending-business
              add 1 to ws-recs-in
           end-if

           .                                                                        
       0120-exit.
           exit.
       0130-build-record.

           move spaces                 to ws-extract-out

           string
              sk-date                ';'
              sk-time                ';'
              jp-user-id             ';'
              jp-file-id             ';'
              jp-program-id          ';'
              jp-record-type         ';'
              pb-carrier             ';'
              pb-state               ';'
              pb-account             ';'
              pb-cert-no             ';'
              pb-record-type         ';'
              pb-force-code          ';'
              '*' delimited by size
              into ws-extract-out
           end-string


      * * *   Need to hold the 'B' record and compare to the 'C'
      * * *   record and if the same do not write extract

           if jp-record-type = 'B'
              perform 0020-build-previous
                                       thru 0020-exit
              move ws-extract-out      to ws-held-record
              set holding-record to true
              go to 0130-exit
           end-if

           if jp-record-type = 'C'
              if holding-record
                 and jp-user-id = ps-user-id
                 and jp-file-id = ps-file-id
                 and jp-program-id = ps-program-id
                 and ps-record-type = 'B'
                 and pb-record-body = ps-record-body
                 display ' Bypassing Records ' pb-cert-no
                 add +1 to ws-recs-byp
                 move ' ' to ws-hold-rec-ind
                 go to 0130-exit
              else
                 move ' ' to ws-hold-rec-ind
                 move ws-held-record   to file-out-rec
                 perform 0140-write-record
                                       thru 0140-exit
              end-if
           end-if

           move ws-extract-out         to file-out-rec
           perform 0140-write-record   thru 0140-exit

           .
       0130-exit.
           exit.

       0140-write-record.

           write file-out-rec
           add +1 to ws-recs-out

           .
       0140-exit.
           exit.

       0510-EOJ-CLOSER.                                                 

           display '  records in       ' ws-recs-in
           display '  records out      ' ws-recs-out
           display '  records bypassed ' ws-recs-byp

                                                                        
           CLOSE pb-journal-in
                 file-out
                                                                        
           GOBACK.                                                      
     
       8500-DATE-CONVERT.
     
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
     
           .
       8500-EXIT.
           EXIT.
     
       ABEND-PGM SECTION.              COPY ELCABEND.                   
