       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID.                ECS072AS.
      *AUTHOR.     Pablo
      *            Colleyville, TX.
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
                                                                        
      *REMARKS. THIS PROGRAM REPORTS THE RATES JOURNAL FILE
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT RT-JOURNAL-IN    ASSIGN TO SYS010.
           SELECT PRINTX           ASSIGN TO SYS008.
           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT SORT-WORK        ASSIGN TO SORTWK1.
           SELECT FICH             ASSIGN TO SYS020.
     
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  RT-JOURNAL-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  RT-JOURNAL-IN-REC.
           05  filler                  pic x(34).
           05  rt-jp-rest-of-rec       pic x(2000).

       FD  PRINTX                                                       
                                       COPY ELCPRTFD.                   

       FD  FICH                                                         
                                       COPY ELCFCHFD.                   

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.                   

       SD  SORT-WORK.                              
                                                   
       01  SORT-REC.                               
           05  sort-key.
               10  sk-date             pic 99999999.
               10  sk-state            pic xx.
               10  sk-class            pic xx.
               10  sk-dev              pic xxx.
               10  sk-ben-type         pic x.
               10  sk-ben-code         pic xx.
               10  sk-age              pic 99.
               10  sk-amt              pic 999999.
               10  sk-exp-dt           pic 99999999.
               10  sk-action           pic x.
               10  sk-time             pic 9(7).
           05  sort-user               pic x(4).
           05  sort-program            pic x(8).
           05  sort-rest-of-record     pic x(1965).
                                                   
       WORKING-STORAGE SECTION.                                         
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '    ECS072AS WORKING-STORAGE    '.  
       77  FILLER  PIC X(32) VALUE '*****VMOD=2.001*****************'.  
                                                                        
       77  ACTR                        PIC S99     COMP.                
       77  BCTR                        PIC S99     COMP.                
       77  CTR                         PIC S999    COMP.                
       77  CTS                         PIC S999    COMP.                
       77  CTT                         PIC S999    COMP.                
       77  WK-1                        PIC S999    COMP-3.              
       77  X                           PIC X.                           
       77  T-FLD                       PIC S9(6)V9(5)  COMP-3.          
       77  PAGER                       PIC S9(4)   VALUE +1.            
       77  LINE-CNT                    PIC S9(4)   VALUE ZEROS.         
       77  CTA                         PIC 9.                           
       77  MO-RTS                      PIC S999    COMP-3    VALUE +360.
       77  ws-records-processed        pic 9(5) value zeros.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  ws-recs-rel                 pic 9(7) value zeros.     
       77  ws-recs-ret                 pic 9(7) value zeros.     
       77  ws-input-sw                 pic x value spaces.
           88  end-of-input                 value 'Y'.
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
           12  JP-RECORD-AREA          pic x(2000).                               
                                                            
       01  WS-ABEND.                                                    
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
           12  WS-ZERO                 PIC S9          VALUE ZERO.      
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
                                                                        
       01  PRINT-REC.                              
           05  print-key.
               10  pr-date             pic 99999999.
               10  pr-ccyymmdd redefines pr-date.
                   15  pr-dt-ccyy      pic xxxx.
                   15  pr-dt-mm        pic xx.
                   15  pr-dt-dd        pic xx.
               10  pr-state            pic xx.
               10  pr-class            pic xx.
               10  pr-dev              pic xxx.
               10  pr-ben-type         pic x.
               10  pr-ben-code         pic xx.
               10  pr-age              pic 99.
               10  pr-amt              pic 999999.
               10  pr-exp-dt           pic 99999999.
               10  pr-action           pic x.
               10  pr-time             pic 9(7).
           05  print-user              pic x(4).
           05  print-program           pic x(8).
           05  print-rest-of-record    pic x(2000).


       01  WS.                                                          
           12  PGM-SUB                 PIC S999    COMP    VALUE +072.  
           12  RATE-FILE-STATUS        PIC XX          VALUE ZERO.      

                                       COPY ERCRATE
                replacing leading ==rt== by ==pv==
                rate-record by prev-rate-record.

                                       COPY ERCRATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       01  compare-keys.
           05  curr-key                pic x(34).
           05  curr-action             pic x.
           05  prev-key                pic x(34).
           05  prev-action             pic x.

       01  data-line-0.
           12  filler                  pic x(16).
           12  DL0-COMMENT             PIC X(20).
           12  FILLER                  PIC X(5).
           12  DL0-USER                PIC X(4).
           12  FILLER                  PIC X(5).
           12  DL0-DATE                PIC X(10).
           12  FILLER                  PIC X(5).
           12  DL0-TIME                PIC 9(6).
           12  FILLER                  PIC X(5).
           12  DL0-PROGRAM             PIC X(8).
     
       01  DATA-LINE-1.                                                 
           12  FILLER                  PIC X       VALUE SPACE.         
           12  FILLER                  PIC X(6)    VALUE 'STATE '.      
           12  DL-1AA                  PIC XX.                          
           12  FILLER                  PIC XXX     VALUE ' - '.         
           12  DL-1A                   PIC X(30).                       
           12  FILLER                  PIC X(5)    VALUE SPACE.         
           12  DL-1DTE                 PIC X(28).                       
           12  FILLER                  PIC X(5)    VALUE SPACES.        
           12  DL-COMNT                PIC X(50).                       
                                                                        
       01  DATA-LINE-1A.                                                
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  FILLER                  PIC X(8)    VALUE 'CLASS = '.    
           12  DL-CLASS                PIC XX.                          
           12  FILLER                  PIC X(16)                        
               VALUE '    DEVIATION = '.                                
           12  DL-DEV                  PIC XXX.                         
           12  FILLER                  PIC X(15)                        
               VALUE '    HIGH AGE = '.                                 
           12  DL-HI-AGE               PIC XX.                          
           12  FILLER                  PIC X(15)                        
               VALUE '    HIGH AMT = '.                                 
           12  DL-HI-AMT               PIC 999999.                      
                                                                        
       01  DATA-LINE-2.                                                 
           12  FILLER                  PIC X(11)   VALUE '        *'.   
           12  DL-2-OVRD               PIC X(12).                       
           12  DL-2A                   PIC XX.                          
           12  FILLER                  PIC XXX     VALUE ' - '.         
           12  DL-2B                   PIC X(12).                       
           12  FILLER                  PIC X(5)    VALUE SPACES.        
           12  DL-2A-COMNT             PIC X(50).                       
                                                                        
       01  DATA-LINE-4.                                                 
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  DL-4-OVRD               PIC X(06).                       
           12  FILLER      PIC X(11)                                    
               VALUE ' MORTALITY '.                                     
           12  FILLER                  PIC X(7)    VALUE 'CODE = '.     
           12  DL-4C                   PIC X(4)    VALUE SPACES.        
           12  FILLER                  PIC XXX     VALUE ' = '.         
           12  DL-4B                   PIC X(26).                       
                                                                        
       01  DATA-LINE-4A.                                                
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  FILLER      PIC X(22)                                    
               VALUE 'MAXIMUM ATTAINED AGE '.                           
           12  DL-4AGE                 PIC Z9.                          
                                                                        
       01  DATA-LINE-5.                                                 
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  FILLER      PIC X(29)                                    
               VALUE 'EXCEPTIONS     AGE'.                              
           12  DL-5A               OCCURS 8.                            
               16  DL-5B               PIC ZZ.                          
               16  DL-5C               PIC X(8).                        
                                                                        
       01  DATA-LINE-6.                                                 
           12  FILLER                  PIC X(26)   VALUE SPACES.        
           12  FILLER                  PIC X(13)   VALUE 'TERM'.        
           12  DL-6A               OCCURS 8.                            
               16  DL-6B               PIC ZZZ.                         
               16  DL-6C               PIC X(7).                        
                                                                        
       01  DATA-LINE-7.                                                 
           12  FILLER                  PIC X(26)   VALUE SPACES.        
           12  FILLER                  PIC X(9)    VALUE 'FACE'.        
           12  DL-7A               OCCURS 8.                            
               16  DL-7B               PIC Z(7).                        
               16  DL-7C               PIC X(3).                        
                                                                        
       01  DATA-LINE-8.                                                 
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  DL-8-OVRD               PIC X(06).                       
           12  FILLER                  PIC X(17)                        
               VALUE ' RATES BY MONTHS'.                                
                                                                        
       01  DATA-LINE-9.                                                 
           12  FILLER                  PIC X(16)   VALUE SPACES.        
           12  FILLER                  PIC X(10)   VALUE '   1'.        
           12  FILLER                  PIC X(10)   VALUE '   2'.        
           12  FILLER                  PIC X(10)   VALUE '   3'.        
           12  FILLER                  PIC X(10)   VALUE '   4'.        
           12  FILLER                  PIC X(10)   VALUE '   5'.        
           12  FILLER                  PIC X(10)   VALUE '   6'.        
           12  FILLER                  PIC X(10)   VALUE '   7'.        
           12  FILLER                  PIC X(10)   VALUE '   8'.        
           12  FILLER                  PIC X(10)   VALUE '   9'.        
           12  FILLER                  PIC X(10)   VALUE '  10'.        
           12  FILLER                  PIC X(10)   VALUE '  11'.        
           12  FILLER                  PIC X(10)   VALUE '  12'.        
                                                                        
       01  DATA-LINE-10.                                                
           12  FILLER                  PIC X(5)    VALUE SPACES.        
           12  FILLER                  PIC X(4)    VALUE 'YEAR'.        
           12  DL-10A                  PIC ZZ9.                         
           12  DL-10B              OCCURS 12.                           
               16  DL-10C              PIC XX.                          
               16  DL-10D              PIC ZZ.Z(5).                     
           12  FILLER                  PIC XXXX    VALUE SPACES.        
                                                                        
       01  DATA-LINE-11.                                                
           12  FILLER                  PIC X(11)   VALUE '        *'.   
           12  DL-11-OVRD              PIC X(12).                       
           12  FILLER                  PIC X(5)    VALUE 'TYPE'.        
           12  DL-11A                  PIC XX.                          
           12  FILLER                  PIC XXX     VALUE ' - '.         
           12  DL-11B                  PIC X(12).                       
           12  FILLER                  PIC X(5)    VALUE SPACES.        
           12  DL-11A-COMM             PIC X(50).                       
                                                                        
       01  DATA-LINE-12.                                                
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  FILLER                  PIC X(29)                        
               VALUE 'EXCEPTIONS     AGE'.                              
           12  DL-12A              OCCURS 8.                            
               16  DL-12B              PIC ZZ.                          
               16  DL-12C              PIC X(8).                        
                                                                        
       01  DATA-LINE-12A.                                               
           12  FILLER                  PIC X(26)   VALUE SPACES.        
           12  FILLER                  PIC X(13)   VALUE 'TERM'.        
           12  DL-12A-TERM     OCCURS 8.                                
               16  DL-12A-T            PIC ZZZ.                         
               16  DL-12A-FIL          PIC X(7).                        
                                                                        
                                                                        
       01  DATA-LINE-13.                                                
           12  FILLER                  PIC X(26)   VALUE SPACES.        
           12  FILLER                  PIC X(11)   VALUE 'BENEFIT'.     
           12  DL-13A              OCCURS 8.                            
               16  DL-13B              PIC Z(5).                        
               16  DL-13C              PIC X(5).                        
                                                                        
       01  DATA-LINE-13A.                                               
           12  FILLER                  PIC X(26)   VALUE SPACES.        
           12  FILLER                  PIC X(9)    VALUE 'FACE'.        
           12  DL-13A-BENF     OCCURS 8 TIMES.                          
               16  DL-13A-BF           PIC Z(7).                        
               16  DL-13A-FIL          PIC X(3).                        
                                                                        
       01  DATA-LINE-14.                                                
           12  FILLER                  PIC X(11)   VALUE SPACES.        
           12  DL-14-OVRD              PIC X(06).                       
           12  FILLER                  PIC X(18)                        
               VALUE ' RATES BY MONTHS'.                                
                                                                        
       01  TRAILER-LINE.                                                
           12  FILLER                  PIC X(110) VALUE SPACES.         
           12  TRAILER-DESC            PIC X(12)  VALUE SPACES.         
           12  TRAILER-DETL            PIC X(8)   VALUE SPACES.         
                                                                        
       EJECT                                                            
       01  HEAD-1.                                                      
           12  FILLER                  PIC X(45)   VALUE SPACES.        
           12  H1-OVRD-1               PIC X(12).                       
           12  FILLER                  PIC X(05)   VALUE ' AND '.       
           12  H1-OVRD-2               PIC X(12).                       
           12  FILLER                  PIC X(06)   VALUE ' RATES'.      
           12  FILLER                  PIC X(39)   VALUE SPACES.        
           12  FILLER                  PIC X(8)    VALUE 'ECS072 '.     
                                                                        
       01  HEAD-2.                                                      
           12  FILLER                  PIC X(47)   VALUE SPACES.        
           12  H1-A                    PIC X(30).                       
           12  FILLER                  PIC X(42)   VALUE SPACES.        
           12  H2-DATE                 PIC X(8).                        
                                                                        
       01  HEAD-2A.                                                     
           12  FILLER                  PIC X(53)   VALUE SPACES.        
           12  H2A-DATE                PIC X(18).                       
           12  FILLER                  PIC X(48)   VALUE SPACES.        
           12  FILLER                  PIC X(5)    VALUE 'PAGE '.       
           12  DL-1D                   PIC ZZ,ZZ9.                      
                                                                        
       EJECT                                                            
       01  ws-date.
           05  ws-audit-report-dt      pic 9(8) value zeros.

       01  ws-julian-rpt-dt            pic 9(5) value zeros.
       01  PRETTY-DATE.                                                 
           12  PRETTY-MO               PIC Z9.                          
           12  FILLER                  PIC X       VALUE '-'.           
           12  PRETTY-DAY              PIC 99.                          
           12  FILLER                  PIC X       VALUE '-'.           
           12  PRETTY-YR               PIC 99.                          
                                                                        
       01  L1-CUR-DTE.                                                  
           12  FILLER      PIC X(27)                                    
               VALUE 'THESE ARE THE CURRENT RATES'.                     
                                                                        
       01  L1-DATER.                                                    
           12  FILLER                  PIC X(20)                        
               VALUE 'THESE RATES EXPIRED'.                             
           12  L1D-FILL                PIC X(8).                        
     
                                       COPY ELCDATE.
     
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH BINARY  PICTURE IS S9(4).
           05  PARM-VALUE  DISPLAY PICTURE IS X(8).

       PROCEDURE DIVISION USING PARM.

                                       COPY ELCDTERX.

           perform 0000-open-files     thru 0000-exit
           perform 0010-init           thru 0010-exit

           sort sort-work on ascending key sort-key
              input procedure is  0100-sort-input
                                       thru 0100-exit
              output procedure is 0200-sort-output
                                       thru 0200-exit
           go to 0510-EOJ-CLOSER

           goback

           .                                                                        
       0000-OPEN-FILES.                                                 
                                                                        
           OPEN INPUT RT-JOURNAL-IN
               OUTPUT PRINTX
           .
       0000-exit.
           exit.

       0010-init.

           move zeros                  to ws-date
                                          ws-julian-rpt-dt

           display ' parm length ' parm-length
           display ' parm value ***' parm-value '***'
           if parm-length > zeros
              and parm-value numeric
              move parm-value          to ws-date
           end-if

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

           MOVE WS-CURRENT-DATE        TO H2-DATE
           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD

           MOVE COMPANY-NAME           TO H1-A
           MOVE ALPH-DATE              TO H2A-DATE
           MOVE CLAS-STARTM            TO CLAS-INDEXM

           perform varying ctr from +1 by +1 until ctr > 8
              move spaces              to dl-5c (ctr)
                                          dl-6c (ctr)
                                          dl-7c (ctr)
                                          dl-10c (ctr)
                                          dl-12c (ctr)
                                          dl-13c (ctr)
          end-perform

           perform varying ctr from +9 by +1 until ctr > 12
              move spaces              to dl-10c (ctr)
          end-perform

           .
       0010-exit.
           exit.

       0100-sort-input.

           display ' **** begin sort input routine **** '
           perform 0120-read-input     thru 0120-exit
           perform 0110-process-input  thru 0110-exit until
              end-of-input

           display ' **** end of sort input routine **** '
           display ' records released ' ws-recs-rel

           .
       0100-exit.
           exit.

       0110-process-input.

           if rt-company-cd not = dte-clasic-company-cd
              go to 0110-continue
           end-if

           if ws-julian-rpt-dt = zeros
              continue
           else
              if jp-date not = ws-julian-rpt-dt
                 go to 0110-continue
              end-if
           end-if

           move jp-date                to dc-julian-date
           move '5'                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-cymd   to sk-date
           else
              move zeros               to sk-date
           end-if
           move rt-st-code             to sk-state
           move rt-st-class            to sk-class
           move rt-st-dev              to sk-dev
           move rt-l-ah                to sk-ben-type
           move rt-lah-num             to sk-ben-code
           move rt-high-age            to sk-age
           move rt-high-amt            to sk-amt
           move rt-expiry-date         to sk-exp-dt
           move jp-record-type         to sk-action
           move jp-time                to sk-time
           move jp-user-id             to sort-user
           move jp-program-id          to sort-program
           move rate-record            to sort-rest-of-record
           release sort-rec
           add 1 to ws-recs-rel

           .
       0110-continue.

           perform 0120-read-input     thru 0120-exit

           .
       0110-exit.
           exit.

       0120-READ-input.
                                                                        
           READ RT-JOURNAL-IN AT END
              set end-of-input         to true
           END-READ
     
           if not end-of-input
              MOVE rt-jp-rest-of-rec   TO JOURNAL-RECORD
              MOVE JP-RECORD-AREA      TO RATE-record
              add 1 to ws-recs-in
           end-if
     
                                       COPY ELCRTEM1.

           .                                                                        
       0120-exit.
           exit.

       0200-sort-output.

           display ' **** begin sort output routine **** '
           move ' '                    to ws-input-sw
           perform 0220-return-rec     thru 0220-exit
           move curr-key               to prev-key
           move curr-action            to prev-action
           move sort-rec               to print-rec
           perform 0210-process-output thru 0210-exit until
              end-of-input

           if ws-recs-ret not = zeros
              perform 0215-key-change  thru 0215-exit
           end-if

           display ' **** end of sort output routine **** '
           display ' records returned ' ws-recs-ret

           .
       0200-exit.
           exit.

       0210-process-output.

           evaluate true
              when curr-key not = prev-key
                 perform 0215-key-change
                                       thru 0215-exit
                 move curr-key         to prev-key
                 move curr-action      to prev-action
                 move sort-rec         to print-rec
              when curr-action not = prev-action
                 perform 0217-action-change
                                       thru 0217-action-change
                 move sort-rec         to print-rec
      ****  right here I want to use the oldest B (before) image to
      ***   go on the report.
              when prev-action = 'B'
                   and curr-action = 'B'
                   continue
      ****  I want to use the newest C (after) image to
      ***   go on the report.
              when other
                 move sort-rec         to print-rec
           end-evaluate

      *    if curr-key not = prev-key
      *       perform 0215-key-change  thru 0215-exit
      *       move curr-key            to prev-key
      *       move curr-action         to prev-action
      *       move sort-rec            to print-rec
      *    else
      *      if curr-action not = prev-action
      *         perform 0217-action-change thru 0217-action-change
      *         move sort-rec          to print-rec
      *      else
      *         if prev-action = 'B'
      *            and curr-action = 'B'
      *            continue
      *         else
      *            move sort-rec       to print-rec
      *         end-if
      *      end-if
      *    end-if
      *
      *    move sort-rec               to print-rec

           perform 0220-return-rec     thru 0220-exit

           .
       0210-exit.
           exit.

       0215-key-change.

           evaluate true
              when prev-action = 'C'
                 perform 1000-print-rpt thru 1000-exit
      *          we need to print the after image here
              when prev-action = 'A'
                 perform 1000-print-rpt thru 1000-exit
      *          we need to print the add image here
              when prev-action = 'D'
                 perform 1000-print-rpt thru 1000-exit
      *          we need to print the delete image here
              when other
                 display ' something not thought out key chg '
                    prev-action
           end-evaluate

           .
       0215-exit.
           exit.

       0217-action-change.

           evaluate true
              when (prev-action = 'A')
                 and (curr-action = 'B' OR 'C')
                 continue
              when prev-action = 'B'
                 perform 1000-print-rpt thru 1000-exit
      *          WE NEED TO PRINT THE before image here
                 move curr-action      to prev-action
              when other
                 display ' something not thought out - action chg '
                    prev-action
           end-evaluate

           .
       0217-exit.
           exit.

       0220-return-rec.

           return sort-work at end
              set end-of-input         to true
           end-return

           if not end-of-input
              move sort-key (1:34)     to curr-key
              move sk-action           to curr-action
              add 1                    to ws-recs-ret
           end-if

           .
       0220-exit.
           exit.

       1000-print-rpt.

           move print-rest-of-record   to rate-record
           MOVE RT-ST-CODE             TO DL-1AA.                       
           MOVE RT-ST-DEV              TO DL-DEV.                       
           MOVE RT-STATE-CODE          TO STATE-L.                      

           .
       0160-LOOK-UP-STATE.                                              
                                   COPY ECSSTLOK.                       
                                                                        
           MOVE STATE-PIC (CLAS-INDEXS) TO DL-1A.                       
                                                                        
           IF RT-EXP-DA = 99                                            
               MOVE L1-CUR-DTE         TO DL-1DTE                       
           ELSE                                                         
               MOVE RT-EXP-YR          TO PRETTY-YR                     
               MOVE RT-EXP-MO          TO PRETTY-MO                     
               MOVE RT-EXP-DA          TO PRETTY-DAY                    
               MOVE PRETTY-DATE        TO L1D-FILL                      
               MOVE L1-DATER           TO DL-1DTE.                      
                                                                        
           MOVE PAGER                  TO DL-1D.                        
           ADD 1 TO PAGER.                                              
           MOVE ZEROS                  TO LINE-CNT.                     
           MOVE '1'                    TO X.                            
           MOVE LIFE-OVERRIDE-L12      TO H1-OVRD-1.                    
           MOVE   AH-OVERRIDE-L12      TO H1-OVRD-2.                    
           MOVE HEAD-1                 TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE HEAD-2                 TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE HEAD-2A                TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '0'                    TO X.                            
     
           move print-user             to dl0-user
           move print-program          to dl0-program
           move pr-time                to dl0-time

           string pr-dt-mm '/' pr-dt-dd '/' pr-dt-ccyy
              delimited by size into dl0-date
           end-string
      *    move pr-date                to dl0-date
      *                                   dc-julian-date
      *    move '5'                    to dc-option-code
      *    perform 8500-date-convert   thru 8500-exit
      *    if no-conversion-error
      *       move dc-greg-date-a-edit to dl0-date
      *    else
      *       move 'xx/xx/xxxx'        to dl0-date
      *    end-if
           evaluate prev-action
              when 'A'
                 MOVE 'NEW RECORD IMAGE'
                                       TO DL0-COMMENT
              WHEN 'B'
                 MOVE 'BEFORE IMAGE'   TO DL0-COMMENT
              WHEN 'C'
                 MOVE 'AFTER IMAGE'    TO DL0-COMMENT
              WHEN 'D'
                 MOVE 'DELETED IMAGE'  TO DL0-COMMENT
              WHEN OTHER
                 MOVE 'UNIDENTIFIED'   TO DL0-COMMENT
           end-evaluate
     
           move data-line-0            to p-data
           perform 0260-prt-rtn        thru 0270-prt-rtn-exit
                                                                        
           MOVE RT-ST-CLASS            TO DL-CLASS.                     
           MOVE RT-HIGH-AGE            TO DL-HI-AGE.
           MOVE RT-HIGH-AMT            TO DL-HI-AMT
           MOVE RT-STRUCTURE-COMMENT   TO DL-COMNT.                     
                                                                        
           MOVE DATA-LINE-1            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE DATA-LINE-1A           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '0'                    TO X.                            
           IF RT-L-AH NOT = 'L'                                         
               GO TO 0220-DOING-A-AND-H.                                
                                                                        
       0170-PRINT-THIS-LIFE.                                            
                                                                        
           MOVE RT-LAH-NUM             TO DL-2A.                        
           MOVE LIFE-OVERRIDE-L12      TO DL-2-OVRD.                    
           PERFORM 0440-FIND-L-NAME THRU 0460-E-F-L-NAME.               
           MOVE RT-RATE-COMMENT        TO DL-2A-COMNT.                  
           MOVE DATA-LINE-2            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE SPACES                 TO DL-4B.                        
                                                                        
           IF RT-LIFE-MORT-CODE NOT = CLAS-MORT-CODE (CLAS-INDEXM)      
               MOVE CLAS-STARTM        TO CLAS-INDEXM.                  
                                                                        
       0180-MORT-DESC-LOOP.                                             
                                                                        
           IF RT-LIFE-MORT-CODE = CLAS-MORT-CODE (CLAS-INDEXM)          
               MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO DL-4B               
           ELSE                                                         
               ADD +1 TO CLAS-INDEXM                                    
               IF CLAS-INDEXM NOT GREATER THAN CLAS-MAXM                
                   GO TO 0180-MORT-DESC-LOOP.                           
                                                                        
           MOVE LIFE-OVERRIDE-L6       TO DL-4-OVRD.                    
           MOVE RT-LIFE-MORT-CODE      TO DL-4C.                        
           MOVE DATA-LINE-4            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE RT-MAX-AGE             TO DL-4AGE.                      
           MOVE DATA-LINE-4A           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTS.                          
           PERFORM 0320-L-X-AGE 8 TIMES.                                
           MOVE DATA-LINE-5            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTS.                          
           PERFORM 0340-L-X-TERM 8 TIMES.                               
           MOVE DATA-LINE-6            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTS.                          
           PERFORM 0350-L-X-FACE 8 TIMES.                               
           MOVE DATA-LINE-7            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '-'                    TO X.                            
           MOVE LIFE-OVERRIDE-L6       TO DL-8-OVRD.                    
           MOVE DATA-LINE-8            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '0'                    TO X.                            
           MOVE DATA-LINE-9            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTT.                          
                                                                        
       0190-CALC-YEAR-NUMBER-L.                                         
                                                                        
           COMPUTE WK-1 = (CTT + 11) / 12.                              
           MOVE WK-1                   TO DL-10A.                       
           MOVE CTT                    TO CTS.                          
           MOVE ZERO                   TO T-FLD.                        
           PERFORM 0330-LIFE-RATE-ADD 12 TIMES.                         
                                                                        
           IF T-FLD = ZERO                                              
               ADD 12 TO CTT                                            
               GO TO 0200-CK-LIFE-END.                                  
                                                                        
           MOVE 1                      TO CTS.                          
           PERFORM 0360-FILL-LIFE-RATES 12 TIMES.                       
           MOVE DATA-LINE-10           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
                                                                        
       0200-CK-LIFE-END.                                                
                                                                        
           IF CTT NOT GREATER MO-RTS                                    
               GO TO 0190-CALC-YEAR-NUMBER-L.                           
                                                                        
           PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    
           go to 1000-exit
      *    GO TO 0150-READ-MASTER-RATE.                                 
                                                                        
           .
       0220-DOING-A-AND-H.                                              
                                                                        
           MOVE AH-OVERRIDE-L12        TO DL-11-OVRD.                   
           MOVE RT-LAH-NUM             TO DL-11A.                       
           PERFORM 0470-FIND-D-NAME THRU 0490-E-F-D-NAME.               
           MOVE RT-RATE-COMMENT        TO DL-11A-COMM.                  
           MOVE DATA-LINE-11           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE RT-MAX-AGE             TO DL-4AGE.                      
           MOVE DATA-LINE-4A           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTS.                          
           PERFORM 0380-AH-X-AGE 8 TIMES.                               
           MOVE DATA-LINE-12           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1 TO CTS.                                               
           PERFORM 0390-AH-X-TERM 8 TIMES.                              
           MOVE DATA-LINE-12A          TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTS.                          
           PERFORM 0400-AH-X-BEN 8 TIMES.                               
           MOVE DATA-LINE-13           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1 TO CTS.                                               
           PERFORM 0410-AH-X-FACE 8 TIMES.                              
           MOVE DATA-LINE-13A          TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '-'                    TO X.                            
           MOVE AH-OVERRIDE-L6         TO DL-14-OVRD.                   
           MOVE DATA-LINE-14           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE '0'                    TO X.                            
           MOVE DATA-LINE-9            TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 1                      TO CTT.                          
                                                                        
       0230-CALC-YEAR-NUMBER-AH.                                        
                                                                        
           COMPUTE WK-1 = (CTT + 11) / 12.                              
           MOVE WK-1                   TO DL-10A.                       
           MOVE CTT                    TO CTS.                          
           MOVE ZERO                   TO T-FLD.                        
           PERFORM 0370-AH-RATE-ADD 12 TIMES.                           
                                                                        
           IF T-FLD = ZERO                                              
               ADD 12 TO CTT                                            
               GO TO 0240-CK-AH-END.                                    
                                                                        
           MOVE 1                      TO CTS.                          
           PERFORM 0420-FILL-AH-RATES 12 TIMES.                         
           MOVE DATA-LINE-10           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
                                                                        
       0240-CK-AH-END.                                                  
                                                                        
           IF CTT NOT GREATER MO-RTS                                    
               GO TO 0230-CALC-YEAR-NUMBER-AH.                          
                                                                        
           PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    
      *    GO TO 0150-READ-MASTER-RATE.                                 

           .                                                                        
       1000-exit.
           exit.

       0260-PRT-RTN.                                                    
                                       COPY ELCPRT2.                    
           ADD 1 TO LINE-CNT.                                           
                                                                        
           IF X = '0'                                                   
               ADD 1 TO LINE-CNT                                        
           ELSE                                                         
               IF X = '-'                                               
                   ADD 2 TO LINE-CNT.                                   
                                                                        
           MOVE ' '                    TO X.                            
                                                                        
       0270-PRT-RTN-EXIT.    EXIT.                                      
                                                                        
       EJECT                                                            
       0290-TRAILER-ROUTINE.                                            
                                                                        
           IF LINE-CNT LESS 48                                          
               MOVE SPACES             TO P-DATA                        
               PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT              
               GO TO 0290-TRAILER-ROUTINE.                              
                                                                        
           MOVE 'STATE'                TO TRAILER-DESC.                 
           MOVE RT-ST-CODE             TO TRAILER-DETL.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 'CLASS'                TO TRAILER-DESC.                 
           MOVE RT-ST-CLASS            TO TRAILER-DETL.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 'DEVIATION'            TO TRAILER-DESC.                 
           MOVE RT-ST-DEV              TO TRAILER-DETL.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 'TYPE'                 TO TRAILER-DESC.                 
           MOVE RT-L-AH-CODE           TO TRAILER-DETL.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE 'HIGH AGE'             TO TRAILER-DESC.                 
           MOVE RT-HIGH-AGE            TO TRAILER-DETL.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
           MOVE RT-EXP-YR              TO PRETTY-YR.                    
           MOVE RT-EXP-MO              TO PRETTY-MO.                    
           MOVE RT-EXP-DA              TO PRETTY-DAY.                   
           MOVE PRETTY-DATE            TO TRAILER-DETL.                 
           MOVE 'EXP DATE'             TO TRAILER-DESC.                 
           MOVE TRAILER-LINE           TO P-DATA.                       
           PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
                                                                        
       0300-TRAILER-ROUTINE-X.     EXIT.                                
                                                                        
       EJECT                                                            
       0320-L-X-AGE.                                                    
                                                                        
           MOVE RT-L-EX-AGE (CTS)      TO DL-5B (CTS).                  
           ADD 1 TO CTS.                                                
                                                                        
       0330-LIFE-RATE-ADD.                                              
                                                                        
           ADD RT-L-RATE (CTS)         TO T-FLD.                        
           ADD 1 TO CTS.                                                
                                                                        
       0340-L-X-TERM.                                                   
                                                                        
           MOVE RT-L-EX-TERM (CTS)     TO DL-6B (CTS).                  
           ADD 1 TO CTS.                                                
                                                                        
       0350-L-X-FACE.                                                   
                                                                        
           MOVE RT-L-EX-FACE (CTS)     TO DL-7B (CTS).                  
           ADD 1 TO CTS.                                                
                                                                        
       0360-FILL-LIFE-RATES.                                            
                                                                        
           MOVE RT-L-RATE (CTT)        TO DL-10D (CTS).                 
           ADD 1 TO CTT.                                                
           ADD 1 TO CTS.                                                
                                                                        
       0370-AH-RATE-ADD.                                                
                                                                        
           ADD RT-AH-RATE (CTS)        TO T-FLD.                        
           ADD 1 TO CTS.                                                
                                                                        
       0380-AH-X-AGE.                                                   
                                                                        
           MOVE RT-AH-AGE (CTS)        TO DL-12B (CTS).                 
           ADD 1 TO CTS.                                                
                                                                        
       0390-AH-X-TERM.                                                  
                                                                        
           MOVE RT-AH-TERM (CTS)       TO DL-12A-T (CTS).               
           MOVE SPACES                 TO DL-12A-FIL (CTS).             
           ADD 1 TO CTS.                                                
                                                                        
       0400-AH-X-BEN.                                                   
                                                                        
           MOVE RT-AH-BEN-M (CTS)      TO DL-13B (CTS).                 
           ADD 1 TO CTS.                                                
                                                                        
       0410-AH-X-FACE.                                                  
                                                                        
           MOVE RT-AH-BEN-F (CTS)      TO DL-13A-BF (CTS).              
           MOVE SPACES                 TO DL-13A-FIL (CTS).             
           ADD 1 TO CTS.                                                
                                                                        
       0420-FILL-AH-RATES.                                              
                                                                        
           MOVE RT-AH-RATE (CTT)       TO DL-10D (CTS).                 
           ADD 1 TO CTT.                                                
           ADD 1 TO CTS.                                                
                                                                        
       EJECT                                                            
       0440-FIND-L-NAME.                                                
                                                                        
           MOVE SPACES                 TO DL-2B.                        
                                                                        
           IF CLAS-STARTL = ZERO                                        
               GO TO 0460-E-F-L-NAME.                                   
                                                                        
           MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
                                                                        
       0450-FLN-LOOP.                                                   
                                                                        
           IF CLAS-I-BEN (CLAS-INDEXL) = RT-LAH-NUM                     
               MOVE CLAS-I-AB10 (CLAS-INDEXL) TO DL-2B                  
               GO TO 0460-E-F-L-NAME.                                   
                                                                        
           ADD +1 TO CLAS-INDEXL.                                       
                                                                        
           IF CLAS-INDEXL NOT GREATER THAN CLAS-MAXL                    
               GO TO 0450-FLN-LOOP.                                     
                                                                        
       0460-E-F-L-NAME.     EXIT.                                       
                                                                        
                                                                        
       0470-FIND-D-NAME.                                                
                                                                        
           MOVE SPACES                 TO DL-11B.                       
                                                                        
           IF CLAS-STARTA = ZERO                                        
               GO TO 0490-E-F-D-NAME.                                   
                                                                        
           MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
                                                                        
       0480-FDN-LOOP.                                                   
                                                                        
           IF CLAS-I-BEN (CLAS-INDEXA) = RT-LAH-NUM                     
               MOVE CLAS-I-AB10 (CLAS-INDEXA) TO DL-11B                 
               GO TO 0490-E-F-D-NAME.                                   
                                                                        
           ADD +1 TO CLAS-INDEXA.                                       
                                                                        
           IF CLAS-INDEXA NOT GREATER THAN CLAS-MAXA                    
               GO TO 0480-FDN-LOOP.                                     
                                                                        
       0490-E-F-D-NAME.     EXIT.                                       
                                                                        
       EJECT                                                            
       0510-EOJ-CLOSER.                                                 
     
           if ws-recs-ret = zeros
              MOVE '1'                 TO X
              MOVE LIFE-OVERRIDE-L12   TO H1-OVRD-1
              MOVE   AH-OVERRIDE-L12   TO H1-OVRD-2
              MOVE HEAD-1              TO P-DATA
              PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT
              MOVE HEAD-2              TO P-DATA
              PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT
              MOVE HEAD-2A             TO P-DATA
              PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT
              move '0'                 to X
              move 'No changes made today '
                                       to data-line-0 (17:30)
              move data-line-0         to p-data
              PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT
           end-if
     
                                   COPY ELCPRTC.                        
                                                                        
           CLOSE rt-journal-in
                 PRINTX.                                                
                                                                        
           GOBACK.                                                      
     
       8500-DATE-CONVERT.
     
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
     
           .
       8500-EXIT.
           EXIT.
     
       ABEND-PGM SECTION.              COPY ELCABEND.                   
