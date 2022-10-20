       PROGRAM-ID.     CIDBEX4.       
      *AUTHOR.         CSO.
      *DATE-COMPILED.                                                   
     
      *REMARKS.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2022 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 081122                   PEMA  NEW PROGRAM  
      ******************************************************************
      
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT ELBENE              ASSIGN TO ELBENE                  
                                      ORGANIZATION IS INDEXED
                                      ACCESS IS DYNAMIC
                                      RECORD KEY IS BE-CONTROL-PRIMARY
                                      FILE STATUS IS elbene-file-status. 
                                                                        
                                                                        
           SELECT FILE-OUT            ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE           ASSIGN TO SYS019.   
      
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  ELBENE.                                                      
                                                                        
           COPY ELCBENE.                                                
      
       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FILE-OUT-REC                PIC X(500).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                          CL**2
       WORKING-STORAGE SECTION.                                         
       77  FILLER   PIC X(32) VALUE '********************************'. 
       77  FILLER   PIC X(32) VALUE '*           CIDBEX4            *'. 
       77  FILLER   PIC X(32) VALUE '******** VMOD=2.001*************'. 
                                                                        
       01  MISC-WS.                                                     
           05  PGM-SUB               COMP-3 PIC S9(03)    VALUE +343.           
           05  WS-LINE-COUNT         COMP-3 PIC S9(03)    VALUE +56.
           05  WS-LINE-COUNT-MAX     COMP-3 PIC S9(03)    VALUE +55.
           05  WS-PAGE               COMP-3 PIC S9(05)    VALUE +0.
           05  WS-EOF1                      PIC X(01)     VALUE SPACE.
               88  BENE-EOF                               VALUE 'Y'.
           05  WS-ABEND-MESSAGE             PIC  X(80)    VALUE SPACES.      
           05  WS-ABEND-FILE-STATUS         PIC  X(02)    VALUE ZERO.        
           05  WS-ABEND-PROGRAM             PIC  X(08)    VALUE SPACES.      
           05  WS-RETURN-CODE        COMP   PIC S9(04)    VALUE +0.     
           05  WS-ZERO               COMP-3 PIC S9(01)    VALUE +0.          
           05  ABEND-CODE                   PIC  X(04)    VALUE ZERO.        
           05  ABEND-OPTION                 PIC  X(01)    VALUE 'Y'.         
           05  ELBENE-FILE-STATUS           PIC  X(02)    VALUE ZEROS.                                                   
                   
       01  ELBENE-EXTR-RECORD.
           05  tb-rec-type             pic x.                     
           05  TB-BENE-CD              PIC X(10).                 
           05  TB-BENE-NAME            PIC X(30).                 
           05  TB-BENE-ADDR1           PIC X(30).                 
           05  TB-BENE-ADDR2           PIC X(30).                 
           05  TB-BENE-ADDR3           PIC X(30).                 
           05  TB-BENE-CITY            PIC X(28).                 
           05  TB-BENE-STATE           PIC XX.                    
           05  TB-BENE-ZIP             PIC X(9).                  
           05  TB-BENE-PHONE           PIC X(11).                 
           05  tb-group-checks         pic x.                     
           05  TB-CORR-NAME            PIC X(30).                 
           05  TB-CORR-ADDR1           PIC X(30).                 
           05  TB-CORR-ADDR2           PIC X(30).                 
           05  TB-CORR-ADDR3           PIC X(30).                 
           05  TB-CORR-CITY            PIC X(28).                 
           05  TB-CORR-STATE           PIC XX.                    
           05  TB-CORR-ZIP             PIC X(9).                  
           05  TB-CORR-PHONE           PIC X(11).                 
           05  tb-fax-no               pic x(11).                 
           05  TB-ach-ind              PIC X.                     
           05  TB-ach-routing          PIC X(30).                 
           05  TB-ach-acct-no          PIC X(30).                 
           05  TB-ach-sub-type         PIC XX.                    
           05  TB-ach-email-ind        PIC X.                     
           05  tb-ach-email-addr       pic x(35).                 

                                       COPY ELCDTECX. 
                                       COPY ELCDTEVR. 
                                       COPY ELCDATE.

       PROCEDURE DIVISION.                                              
      
       0000-MAIN.                                                 
           COPY ELCDTERX.     
                                                                        
           OPEN INPUT  ELBENE
                OUTPUT FILE-OUT
                                                                        
           IF ELBENE-FILE-STATUS = '00' OR '97'                                  
              CONTINUE                                                 
           ELSE                                                       
              MOVE ELBENE-FILE-STATUS  TO WS-ABEND-FILE-STATUS         
              MOVE 'ERROR OCCURED OPEN - ELBENE'                       
                                        TO WS-ABEND-MESSAGE             
              PERFORM ABEND-PGM
           END-IF

           perform 2000-start thru 2000-exit
           IF NOT bene-eof
              PERFORM 2020-READ-NEXT-BENE
                                       THRU 2020-EXIT
           end-if
           move spaces to file-out-rec
           string
              'BENE_REC_TYPE;'
              'BENE_CD;'
              'BENE_NAME;'
              'BENE_ADDR1;'
              'BENE_ADDR2;'
              'BENE_ADDR3;'
              'BENE_CITY;'
              'BENE_STATE;'
              'BENE_ZIP;'
              'BENE_PHONE;'
              'BENE_GRP_CHECKS;'
              'CORR_NAME;'
              'CORR_ADDR1;'
              'CORR_ADDR2;'
              'CORR_ADDR3;'
              'CORR_CITY;'
              'CORR_STATE;'
              'CORR_ZIP;'
              'CORR_PHONE;'
              'FAX_NO;'
              'ACH_INDICATOR;'
              'ACH_ROUTING;'
              'ACH_ACCTNO;'
              'ACH_SUB_TYPE;'
              'ACH_EMAIL_IND;'
              'ACH_EMAIL_ADDR;E'
              delimited by size into
                file-out-rec
           end-string
           write file-out-rec

           perform 3000-process thru   3000-exit until
              bene-eof

           CLOSE ELBENE                                                 
                 FILE-OUT

           GOBACK
      
           .
       0000-EXIT.
           EXIT.
      
       2000-start.
      
           MOVE LOW-VALUES              TO BE-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD   TO BE-COMPANY-CD
                                                                        
           START ELBENE KEY NOT < BE-CONTROL-PRIMARY
                                                                        
           IF elbene-file-status = '23' or '10'
               set bene-eof to true
               go to 2000-exit
           END-IF
                                                                        
           IF elbene-file-status NOT = ZERO                                      
               MOVE ELBENE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS         
               MOVE 'ERROR OCCURED START - ELBENE'                      
                                        TO  WS-ABEND-MESSAGE             
               PERFORM ABEND-PGM
           END-IF
      
           .
       2000-EXIT.
           EXIT.
                                                                        
       2020-READ-NEXT-BENE.                                                    
      
           READ ELBENE NEXT RECORD
                                                                        
           if (elbene-file-status = '10' OR '23')
              or (be-company-cd <> dte-clasic-company-cd)
              set bene-eof to true
              go to 2020-exit
           end-if

           IF elbene-file-status NOT = '00'
               MOVE elbene-file-status TO  WS-ABEND-FILE-STATUS
               MOVE 'ERROR OCCURED READ - ELBENE'                       
                                        TO  WS-ABEND-MESSAGE             
               PERFORM ABEND-PGM
           END-IF
      
           .
       2020-EXIT.
           EXIT.
      
       3000-process.

           move spaces                 to elbene-extr-record

           move be-record-type         to tb-rec-type
           move be-beneficiary         to TB-BENE-CD
           inspect be-mail-to-name
              converting '''' to spaces
           move be-mail-to-name        to TB-BENE-NAME
           inspect be-address-line-1
              converting '''' to spaces
           move be-address-line-1      to TB-BENE-ADDR1       
           inspect be-address-line-2
              converting '''' to spaces
           move be-address-line-2      to TB-BENE-ADDR2       
           move be-address-line-3      to TB-BENE-ADDR3       
           inspect be-city
              converting '''' to spaces
           move be-city                to TB-BENE-CITY        
           move be-state               to TB-BENE-STATE       
           move be-zip-code            to TB-BENE-ZIP         
           move be-phone-no            to TB-BENE-PHONE
           move be-group-checks-y-n    to tb-group-checks
           inspect be-mail-to-name2
              converting '''' to spaces
           move be-mail-to-name2       to TB-CORR-NAME        
           inspect be-address-line-12
              converting '''' to spaces
           move be-address-line-12     to TB-CORR-ADDR1       
           inspect be-address-line-22
              converting '''' to spaces
           move be-address-line-22     to TB-CORR-ADDR2       
           inspect be-address-line-32
              converting '''' to spaces
           move be-address-line-32     to TB-CORR-ADDR3       
           inspect be-city2
              converting '''' to spaces
           move be-city2               to TB-CORR-CITY        
           move be-state2              to TB-CORR-STATE       
           move be-zip-code2           to TB-CORR-ZIP         
           move be-phone-no2           to TB-CORR-PHONE
           move be-bsr-fax-num         to tb-fax-no
           inspect be-ach-yes-or-no
              converting low-values to spaces
           move be-ach-yes-or-no       to TB-ach-ind
           inspect be-ach-aba-routing-number
              converting low-values to spaces
           move be-ach-aba-routing-number
                                       to TB-ach-routing
           inspect be-ach-bank-account-number
              converting low-values to spaces
           move be-ach-bank-account-number
                                       to TB-ach-acct-no
           inspect be-ach-sub-type
              converting low-values to spaces
           move be-ach-sub-type        to TB-ach-sub-type
           inspect be-ach-email-yn
              converting low-values to spaces
           move be-ach-email-yn        to tb-ach-email-ind
           inspect be-ach-email-addr
              converting low-values to spaces
           move be-ach-email-addr      to tb-ach-email-addr

           perform 3020-write thru 3020-exit
           PERFORM 2020-READ-NEXT-BENE THRU 2020-EXIT

           .
       3000-EXIT.
           EXIT.
      
       3020-write.
      
           move spaces to file-out-rec
           string
              tb-rec-type        ';'
              tb-bene-cd         ';'
              tb-bene-name       ';'
              tb-bene-addr1      ';'
              TB-BENE-ADDR2      ';'   
              TB-BENE-ADDR3      ';'   
              TB-BENE-CITY       ';'   
              TB-BENE-STATE      ';'   
              TB-BENE-ZIP        ';'   
              TB-BENE-PHONE      ';'   
              TB-group-checks    ';'   
              TB-CORR-NAME       ';'   
              TB-CORR-ADDR1      ';'   
              TB-CORR-ADDR2      ';'   
              TB-CORR-ADDR3      ';'   
              TB-CORR-CITY       ';'   
              TB-CORR-STATE      ';'   
              TB-CORR-ZIP        ';'   
              TB-CORR-PHONE      ';'   
              TB-fax-no          ';'   
              TB-ACH-IND         ';'   
              TB-ACH-ROUTING     ';'   
              TB-ACH-ACCT-NO     ';'   
              TB-ACH-SUB-TYPE    ';'
              TB-ach-email-ind   ';'   
              TB-ach-email-addr  ';E'

              delimited by size into file-out-rec
           end-string

           write file-out-rec
           .
       3020-EXIT.
           EXIT.
      
       8500-DATE-CONVERSION.
      
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
      
           .
       8500-EXIT.
           EXIT.
      
       ABEND-PGM SECTION.                                               
                                       COPY ELCABEND.
      
