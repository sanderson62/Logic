       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DCC019.
      *AUTHOR.      Cowtown.
      *             Omaha, NE.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.                                                         
      *          PRINT PREMIUM AND COMMISSION ANALYSIS.                 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 011116  CR2015082400003  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           C02 IS LCP-CH2                                               
           C03 IS LCP-CH3                                               
           C04 IS LCP-CH4                                               
           C05 IS LCP-CH5                                               
           C06 IS LCP-CH6                                               
           C07 IS LCP-CH7                                               
           C08 IS LCP-CH8                                               
           C09 IS LCP-CH9                                               
           C10 IS LCP-CH10                                              
           C11 IS LCP-CH11                                              
           C12 IS LCP-CH12                                              
           S01 IS LCP-P01                                               
           S02 IS LCP-P02.                                              
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT DETAIL-IN        ASSIGN TO SYS010.                    
           SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
           SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
           SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   

           SELECT ERACCTT                                               
                   ASSIGN TO SYS025-FBA1-ERACCTT                        
                   ORGANIZATION INDEXED                                 
                   ACCESS IS DYNAMIC                                    
                   RECORD KEY AM-CONTROL-PRIMARY                        
                   FILE STATUS ERACCT-FILE-STATUS.                      
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  DETAIL-IN                                                    
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.                                         
                                                                        
       01  extract-rec-in              pic x(91).

       FD  ERACCTT.
                                       COPY ERCACCT.

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       FD  PRNTR                                                        
                                       COPY ELCPRTFD.

       FD  FICH                                                         
                                       COPY ELCFCHFD.
                                                                        

       WORKING-STORAGE SECTION.                                         
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
       01  LCP-ASA                       PIC X.                         
       01  FILLER  PIC X(32) VALUE '********************************'.  
       01  FILLER  PIC X(32) VALUE '     DCC019  WORKING STORAGE    '.  
       01  FILLER  PIC X(32) VALUE '***********VMOD=2.001***********'.  
                                                                        
       77  eracct-file-status              pic xx  value '00'.
       77  lf-stuff                        pic x value spaces.
           88  there-is-lf-stuff             value 'Y'.
       77  ah-stuff                        pic x value spaces.
           88  there-is-ah-stuff             value 'Y'.
       77  eof-sw                          pic x  value spaces.
           88  end-of-input                  value 'Y'.
       77  ws-recs-in                      pic 9(7) value zeros.
       01  SAVE-ACCT-KEY                   PIC X(19)  VALUE SPACES.
       01  X                               PIC X       VALUE SPACE.     
       01  SPACE-N                         PIC X       VALUE '1'.       
       01  SPACE-1                         PIC X       VALUE ' '.       
       01  SPACE-2                         PIC X       VALUE '0'.       
       01  SPACE-3                         PIC X       VALUE '-'.       
       01  X-NET                           PIC S9(9)V99          COMP-3.
       01  X-TOTAL                         PIC S9(9)V99          COMP-3.
       01  a1                              pic s999 comp-3 value +0.
       01  s1                              PIC S999              COMP-3.
       01  s1-max                          pic s999  comp-3 value +0.
       01  X2                              PIC S999              COMP-3.
       01  R1                              PIC S999              COMP-3.
       01  R2                              PIC S999              COMP-3.
       01  RECD-CTR                        PIC S9(4) VALUE +0.          
       01  AH-SW                           PIC S9                COMP-3.
       01  LIFE-SW                         PIC S9                COMP-3.
       01  IND-SW                          PIC S9                COMP-3.
       01  GRP-SW                          PIC S9                COMP-3.
       01  PRT-SW                          PIC S9                COMP-3.
       01  FST-SW                          PIC S9                COMP-3.
       01  PGCTR                           PIC S9(5) value +0    COMP-3.
       01  LNCTR                           PIC S999              COMP-3.
       01  SAVE-X2                         PIC S999              COMP-3.
       01  HAVE-SEQ                        PIC X(20).                   
       01  NEED-SEQ                        PIC X(19).                   
       01  SAVE-NAME                       PIC X(30)   VALUE SPACES.    
       01  SAVE-REPORT-CD-1                PIC X(10)   VALUE SPACES.    
       01  SAVE-GROUP                      PIC X(06).                   
       01  WS-TOTAL-FEES                   PIC S9(9)V99 VALUE +0.
       01  WS-DIS-AMT1                     PIC Z,ZZZ,ZZ9.99-.
       01  WS-DIS-AMT2                     PIC Z,ZZZ,ZZ9.99-.

       01  WS.                                                          
           12  REPORT-OPTIONS              PIC X        VALUE '1'.      
               88  CURRENT-MONTH                        VALUE '1'.      
               88  QTR-TO-DATE                          VALUE '2'.      
               88  YEAR-TO-DATE                         VALUE '3'.      
               88  LAST-TWELVE                          VALUE '4'.      
           12  BEGIN-DATE.                                              
               16  BEGIN-CCYY              PIC 9(04)   VALUE 0.         
               16  BEGIN-CCYR REDEFINES BEGIN-CCYY.                     
                   20  BEGIN-CC            PIC 99.                      
                   20  BEGIN-YR            PIC 99.                      
               16  BEGIN-MO                PIC 99      VALUE 0.         
                   88  1ST-QTR                         VALUES 01 02 03. 
                   88  2ND-QTR                         VALUES 04 05 06. 
                   88  3RD-QTR                         VALUES 07 08 09. 
                   88  4TH-QTR                         VALUES 10 11 12. 
                                                                        
           12  WS-PROCESS-DATE.                                         
               16  FILLER                  PIC 9.                       
               16  WS-PROCESS-CCYYMM.                                   
                   20  W-CC                PIC 99.                      
                   20  W-YR                PIC 99.                      
                   20  W-MO                PIC 99.                      
           12  WS-PROCESS-NUMERIC  REDEFINES                            
               WS-PROCESS-DATE             PIC 9(7).                    
                                                                        
           12  WS-RETURN-CODE              PIC S9(4)   VALUE ZEROS COMP.
           12  WS-ABEND-MESSAGE            PIC X(80).                   
           12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZEROS.     
           12  WS-ZERO                     PIC S9      VALUE +0  COMP-3.
           12  PGM-SUB                     PIC S999    VALUE +019  COMP.
      
      
       01  HD1.                                                         
           12  HD1-RPT-CD-1-HDG        PIC X(10)   VALUE SPACES.        
           12  HD1-FILL                PIC XXX     VALUE '   '.         
           12  HD1-RPT-CD-1            PIC X(10)   VALUE SPACES.        
           12  FILLER                  PIC X(12)   VALUE SPACES.        
           12  FILLER      PIC X(24)   VALUE 'PREMIUM AND COMMISSION D'.
           12  FILLER                  PIC X(11)   VALUE 'ISTRIBUTION'. 
           12  HD1-OPTION              PIC X(18)                        
                              VALUE ' -  CURRENT MONTH '.               
           12  HD1-OPTION-2            PIC X(31)   VALUE SPACES.        
           12  FILLER                  PIC X(8)    VALUE 'ECS019A '.    
                                                                        
       01  HD2.                                                         
           12  SUB-HD2                 PIC X(47)   VALUE SPACES.        
           12  HD-CO                   PIC X(30).                       
           12  FILLER                  PIC X(42)   VALUE SPACES.        
           12  HD-RD                   PIC X(8).                        
                                                                        
       01  HD3.                                                         
           12  SUB-HD3                 PIC X(53)   VALUE SPACES.        
           12  HD-DT                   PIC X(18).                       
           12  FILLER                  PIC X(48)   VALUE SPACES.        
           12  FILLER                  PIC X(5)    VALUE 'PAGE '.       
           12  HD-PAGE                 PIC ZZ,ZZ9.                      
                                                                        
       01  HD4.
           12 HD-DESC                  PIC X(05).
           12 FILLER                   PIC X(123)  VALUE '              
      -    ' * *  P R E M I U M  * *           * * * * * * * *  C O M M 
      -    'I S S I O N  * * * * * * * *               CLAIMS'.
      
       01  HD5.
           12 FILLER                   PIC X  VALUE ' '.
           12 FILLER                   PIC X(123)  VALUE '  PRODUCT     
      -    '      NET CLP          O/W    PGM MGMT FEE    INCENTIVE  CON
      -    'TRACT FEE  CSO ADMIN     GA/SALES EXP      CLAIMS'.

       01  SUB-HEADINGS.                                                
           03 HEAD2.                                                    
              05 FILLER                PIC X(40)   VALUE                
           'CARR  GROUP  STATE                  ACCT'.                  
           03 ACCT-HDA REDEFINES HEAD2.                                 
              05 ACCT-HD2              PIC X(40).                       
           03 ST-HDA REDEFINES HEAD2.                                   
              05 ST-HD2                PIC X(28).                       
              05 FILLER                PIC X(12).                       
           03 CO-HDA REDEFINES HEAD2.                                   
              05 CO-HD2                PIC X(11).                       
              05 FILLER                PIC X(29).                       
           03 CARR-HDA REDEFINES HEAD2.                                 
              05 CARR-HD2              PIC X(4).                        
              05 FILLER                PIC X(36).                       
           03 RPT-CD-1-HD2.                                             
              05 FILLER                PIC X(40)   VALUE                
           'REPORT CODE 1   '.                                          
                                                                        
           03 HEAD3.                                                    
              05 FILLER                PIC XX      VALUE SPACES.        
              05 HD-CARR               PIC X.                           
              05 FILLER                PIC XXX     VALUE SPACES.        
              05 HD-GROUP              PIC X(6).                        
              05 FILLER                PIC X       VALUE SPACES.        
              05 HD-ST                 PIC XX.                          
              05 FILLER                PIC X       VALUE SPACES.
              05 HD-ST-NM              PIC X(15).                       
              05 FILLER                PIC XX      VALUE SPACES.        
              05 HD-ACCT               PIC X(10).                       
           03 ACCT-HDB REDEFINES HEAD3.                                 
              05 ACCT-HD3              PIC X(43).                       
           03 ST-HDB REDEFINES HEAD3.                                   
              05 ST-HD3                PIC X(28).                       
              05 FILLER                PIC X(15).                       
           03 CO-HDB REDEFINES HEAD3.                                   
              05 CO-HD3                PIC X(13).                       
              05 FILLER                PIC X(30).                       
           03 CARR-HDB REDEFINES HEAD3.                                 
              05 CARR-HD3              PIC X(4).                        
              05 FILLER                PIC X(39).                       
           03 RPT-CD-1-HD3.                                             
              05 FILLER                PIC XX      VALUE SPACES.        
              05 HD-RPT-CD-1           PIC X(10).                       
                                                                        
           03 SUMM-HD.                                                  
              05 HD-SUMM-CARR-DESC.                                     
                 07 FILLER             PIC X(8)    VALUE 'CARRIER '.    
                 07 HD-SUMM-CARR       PIC XXX.                         
                 07 HD-SUMM-FINAL-DESC.                                 
                    09 FILLER          PIC X(8)    VALUE '  STATE '.    
                    09 HD-SUMM-ST      PIC XXX.                         
                    09 HD-SUMM-ST-NM   PIC X(15).                       
       EJECT                                                            
       01  life-totals.
           05  lf-net-clp              pic s9(9)v99 comp-3 value +0.
           05  lf-net-ow               pic s9(9)v99 comp-3 value +0.
           05  lf-net-pgm-mgt-fee      pic s9(9)v99 comp-3 value +0.
           05  lf-net-incentive        pic s9(9)v99 comp-3 value +0.
           05  lf-net-cont-fee         pic s9(9)v99 comp-3 value +0.
           05  lf-net-cso-admin-fee    pic s9(9)v99 comp-3 value +0.
           05  lf-net-ga-sales         pic s9(9)v99 comp-3 value +0.
           05  lf-claim-pmts           pic s9(9)v99 comp-3 value +0.

       01  ah-totals.
           05  ah-net-clp              pic s9(9)v99 comp-3 value +0.
           05  ah-net-ow               pic s9(9)v99 comp-3 value +0.
           05  ah-net-pgm-mgt-fee      pic s9(9)v99 comp-3 value +0.
           05  ah-net-incentive        pic s9(9)v99 comp-3 value +0.
           05  ah-net-cont-fee         pic s9(9)v99 comp-3 value +0.
           05  ah-net-cso-admin-fee    pic s9(9)v99 comp-3 value +0.
           05  ah-net-ga-sales         pic s9(9)v99 comp-3 value +0.
           05  ah-claim-pmts           pic s9(9)v99 comp-3 value +0.


       01  ws-extract-rec.
           05  ex-carrier              pic x.
           05  ex-grouping             pic x(6).
           05  ex-clp-state            pic xx.
           05  ex-account              pic x(10).
           05  ex-eff-dte              pic 9(8).
           05  ex-cert-no              pic x(11).
           05  ex-ben-type             pic x.
               88  ben-type-lf           value '1'.
               88  ben-type-ah           value '2'.
           05  ex-ben-code             pic xx.
           05  ex-net-clp              pic s9(9)v99 comp-3 value +0.
           05  ex-net-ow               pic s9(9)v99 comp-3 value +0.
           05  ex-net-pgm-mgt-fee      pic s9(9)v99 comp-3 value +0.
           05  ex-net-incentive        pic s9(9)v99 comp-3 value +0.
           05  ex-net-cont-fee         pic s9(9)v99 comp-3 value +0.
           05  ex-net-cso-admin-fee    pic s9(9)v99 comp-3 value +0.
           05  ex-net-ga-sales         pic s9(9)v99 comp-3 value +0.
           05  ex-claim-pmts           pic s9(9)v99 comp-3 value +0.
           05  ex-acct-state           pic xx.

       01  ws-init-accums.
           05  filler occurs 900.
               10  ia-ben-type         pic x  value spaces.
               10  ia-ben-code         pic xx value spaces.
               10  ia-ben-description  pic x(10) value spaces.
               10  ia-net-clp          pic s9(11)v99 comp-3 value +0.
               10  ia-net-ow           pic s9(11)v99 comp-3 value +0.
               10  ia-net-pgm-mgt-fee  pic s9(11)v99 comp-3 value +0.
               10  ia-net-incentive    pic s9(11)v99 comp-3 value +0.
               10  ia-net-cont-fee     pic s9(11)v99 comp-3 value +0.
               10  ia-net-cso-admin-fee pic s9(11)v99 comp-3 value +0.
               10  ia-net-ga-sales     pic s9(11)v99 comp-3 value +0.
               10  ia-claim-pmts       pic s9(11)v99 comp-3 value +0.

       01  filler.
           05  ws-accums occurs 5.
               10  filler occurs 900.
                   15  aa-ben-type     pic x  value spaces.
                   15  aa-ben-code     pic xx value spaces.
                   15  aa-ben-description
                                       pic x(10) value spaces.
                   15  aa-net-clp      pic s9(11)v99 comp-3 value +0.
                   15  aa-net-ow       pic s9(11)v99 comp-3 value +0.
                   15  aa-net-pgm-mgt-fee
                                       pic s9(11)v99 comp-3 value +0.
                   15  aa-net-incentive
                                       pic s9(11)v99 comp-3 value +0.
                   15  aa-net-cont-fee pic s9(11)v99 comp-3 value +0.
                   15  aa-net-cso-admin-fee
                                       pic s9(11)v99 comp-3 value +0.
                   15  aa-net-ga-sales pic s9(11)v99 comp-3 value +0.
                   15  aa-claim-pmts   pic s9(11)v99 comp-3 value +0.

       01  ws-compare-key              pic x(19) value spaces.
       01  ws-prev-acct-state          pic xx.
       01  ws-prev-key.
           05  ws-pk-carrier           pic x.
           05  ws-pk-group             pic x(6).
           05  ws-pk-clp-state         pic xx.
           05  ws-pk-account           pic x(10).
           05  ws-pk-ben-type          pic x.
           05  ws-pk-ben-code          pic xx.          

       01  ws-current-key.
           05  ws-ck-carrier           pic x.
           05  ws-ck-group             pic x(6).
           05  ws-ck-clp-state         pic xx.
           05  ws-ck-account           pic x(10).
           05  ws-ck-ben-type          pic x.
           05  ws-ck-ben-code          pic xx.          

       01  CUR-ACCT-ST                     PIC XX.
       01  CUR-SEQ.                                                     
           12  CUR-ACCT-CNTL.                                           
               16  CUR-CARR                PIC X.                       
               16  CUR-GROUP               PIC X(6).                    
               16  CUR-ST                  PIC XX.                      
               16  CUR-ACCT                PIC X(10).                   
           12  CUR-IG                      PIC 9.                       
           12  CUR-TYPE.                                                
               16  CUR-TYP                 PIC XX.                      
               16  CUR-OB                  PIC X.                       
                                                                        
                                                                        
       01  save-p-rec                      pic x(133) value spaces.

       01  P-REC.                                                       
           12  P-CCSW                      PIC X.                       
           12  P-LN.                                                    
               16  P-TYPE                  PIC X(03).                   
               16  P-DESC                  PIC X(10).                   
               16  FILLER                  PIC X(02).                   
               16  P-NET-CLP               PIC ZZ,ZZZ,ZZZ.99-.          
               16  P-OVER                  PIC Z,ZZZ,ZZZ.99-.           
               16  P-PGM-MGT-FEE           PIC ZZ,ZZZ,ZZZ.99-.
               16  P-DLR-INC               PIC Z,ZZZ,ZZZ.99-.
               16  P-BANK-FEE              PIC ZZ,ZZZ,ZZZ.99-.
               16  P-CSO-ADMIN             PIC Z,ZZZ,ZZZ.99-.
               16  P-GA-SALES              PIC ZZ,ZZZ,ZZZ.99-.
               16  P-CLAIM                 PIC ZZZ,ZZZ,ZZZ.99-.         
               16  FILLER                  PIC X(08).

                                           COPY ELCDTECX.               
                                           COPY ELCDTEVR.               

       PROCEDURE DIVISION.                                              
                                                                       
                                       COPY ELCDTERX.

           perform 0000-init           thru 0000-exit
           perform 0010-open-files     thru 0010-exit

      *    display '**************life table ********'
      *    perform varying clas-indexl from clas-startl by +1 until
      *       clas-indexl > clas-maxl
      *       display clas-indexl ' ' aa-ben-type (1 clas-indexl) ' '
      *          aa-ben-code (1 clas-indexl)
      *    end-perform
      *
      *    display '****************ah table ********'
      *    perform varying clas-indexa from clas-starta by +1 until
      *       clas-indexa > clas-maxa
      *       display clas-indexa ' ' aa-ben-type (1 clas-indexa) ' '
      *          aa-ben-code (1 clas-indexa)
      *    end-perform

           perform 0050-process-input  thru 0050-exit
                   until end-of-input

           perform 0200-account-break  thru 0200-exit
           perform 0300-state-break    thru 0300-exit
           perform 0400-grouping-break thru 0400-exit
           perform 0500-carrier-break  thru 0500-exit
           perform 0600-final-totals   thru 0600-exit
           perform 0170-close-files    thru 0170-exit
           goback

           .
       0000-init.

           MOVE WS-CURRENT-DATE        TO HD-RD
           MOVE COMPANY-NAME           TO HD-CO
           MOVE ALPH-DATE              TO HD-DT
           MOVE RUN-MO                 TO BEGIN-MO
           COMPUTE BEGIN-CCYY = RUN-CCYY - 1
           MOVE SPACES                 TO CUR-SEQ
           MOVE 1                      TO DTE-TOT-OPT
           MOVE DTE-FMT-OPT            TO  REPORT-OPTIONS

           move spaces                 to ws-prev-key ws-current-key
           move +900                   to s1-max
           perform 0005-init-accums    thru 0005-exit
           move clas-starts            to clas-indexs

           .
       0000-exit.
           exit.

       0005-init-accums.

           perform varying s1 from +1 by +1 until s1 > s1-max
              move spaces              to ia-ben-type          (s1)
                                          ia-ben-code          (s1)
                                          ia-ben-description   (s1)

              move zeros               to ia-net-clp           (s1)
                                          ia-net-ow            (s1)
                                          ia-net-pgm-mgt-fee   (s1)
                                          ia-net-incentive     (s1)
                                          ia-net-cont-fee      (s1)
                                          ia-net-cso-admin-fee (s1)
                                          ia-net-ga-sales      (s1)
                                          ia-claim-pmts        (s1)
           end-perform

           perform varying clas-indexl from clas-startl by +1 until
              clas-indexl > clas-maxl
              move '1'                 to ia-ben-type (clas-indexl)
              move clas-i-ben  (clas-indexl)
                                       to ia-ben-code (clas-indexl)
              move clas-i-ab10 (clas-indexl)
                                  to ia-ben-description (clas-indexl)
           end-perform

           perform varying clas-indexa from clas-starta by +1 until
              clas-indexa > clas-maxa
              move '2'                 to ia-ben-type (clas-indexa)
              move clas-i-ben  (clas-indexa)
                                       to ia-ben-code (clas-indexa)
              move clas-i-ab10 (clas-indexa)
                                  to ia-ben-description (clas-indexa)
           end-perform

           move ws-init-accums         to ws-accums (1)
                                          ws-accums (2)
                                          ws-accums (3)
                                          ws-accums (4)
                                          ws-accums (5)

           .
       0005-exit.
           exit.

       0010-open-files.

           OPEN INPUT DETAIL-IN ERACCTT
                output prntr

           IF ERACCT-FILE-STATUS  = '00' OR '97'
              continue
           ELSE
              MOVE +0302               TO WS-RETURN-CODE
              MOVE 'ERROR OPENING ACCT MSTR'
                                       TO WS-ABEND-MESSAGE
              GO TO ABEND-PGM
           end-if

           perform 0020-read-input     thru 0020-exit
           move ws-current-key         to ws-prev-key

           .
       0010-exit.
           exit.

       0020-read-input.

           read detail-in into ws-extract-rec at end
              set end-of-input to true
           end-read

           if not end-of-input
              move ws-extract-rec (1:19)
                                       to ws-current-key
              move ex-ben-type         to ws-ck-ben-type
              move ex-ben-code         to ws-ck-ben-code
              add 1 to ws-recs-in
           end-if

           .
       0020-exit.
           exit.

       0050-process-input.

           if ws-prev-key (1:19) not = ws-current-key (1:19)
      *       display ' key break *' ws-prev-key (1:19) '*'
      *       ws-current-key (1:19)
              perform 0100-key-break   thru 0100-exit
              move ws-current-key      to ws-prev-key
           end-if

           move ex-acct-state          to ws-prev-acct-state
           if ws-ck-ben-type = '2'  *>   AH coverage
              go to 0050-process-ah
           end-if

           if ws-ck-ben-code not = aa-ben-code (1 clas-indexl)
              perform varying clas-indexl from clas-startl by +1 until
                 (ws-ck-ben-code = aa-ben-code (1 clas-indexl))
                 or (clas-indexl > clas-maxl)
              end-perform
              if clas-indexl > clas-maxl
                 display ' we have a problem in 0050 ' ws-ck-ben-code
                    ' ' ws-ck-ben-type ' ' ws-recs-in
                 perform abend-pgm
              end-if
           end-if

           compute aa-net-clp (1 clas-indexl) =
              aa-net-clp (1 clas-indexl) + ex-net-clp
           compute aa-net-ow (1 clas-indexl) =
              aa-net-ow (1 clas-indexl) + ex-net-ow
           compute aa-net-pgm-mgt-fee (1 clas-indexl) =
              aa-net-pgm-mgt-fee (1 clas-indexl) +
              ex-net-pgm-mgt-fee
           compute aa-net-incentive (1 clas-indexl) =
              aa-net-incentive (1 clas-indexl) + ex-net-incentive
           compute aa-net-cont-fee (1 clas-indexl) =
              aa-net-cont-fee (1 clas-indexl) + ex-net-cont-fee
           compute aa-net-cso-admin-fee (1 clas-indexl) =
              aa-net-cso-admin-fee (1 clas-indexl) +
              ex-net-cso-admin-fee
           compute aa-net-ga-sales (1 clas-indexl) =
              aa-net-ga-sales (1 clas-indexl) + ex-net-ga-sales
           compute aa-claim-pmts (1 clas-indexl) =
              aa-claim-pmts (1 clas-indexl) + ex-claim-pmts

           go to 0050-continue

           .
       0050-process-ah.
       
           if ws-ck-ben-code not = aa-ben-code (1 clas-indexa)
              perform varying clas-indexa from clas-starta by +1 until
                 (ws-ck-ben-code = aa-ben-code (1 clas-indexa))
                 or (clas-indexa > clas-maxa)
              end-perform
              if clas-indexa > clas-maxa
                 display ' we have a problem in 0050 ' ws-ck-ben-code
                    ' ' ws-ck-ben-type ' ' ws-recs-in
                 perform abend-pgm
              end-if
           end-if

           compute aa-net-clp (1 clas-indexa) =
              aa-net-clp (1 clas-indexa) + ex-net-clp
           compute aa-net-ow (1 clas-indexa) =
              aa-net-ow (1 clas-indexa) + ex-net-ow
           compute aa-net-pgm-mgt-fee (1 clas-indexa) =
              aa-net-pgm-mgt-fee (1 clas-indexa) +
              ex-net-pgm-mgt-fee
           compute aa-net-incentive (1 clas-indexa) =
              aa-net-incentive (1 clas-indexa) + ex-net-incentive
           compute aa-net-cont-fee (1 clas-indexa) =
              aa-net-cont-fee (1 clas-indexa) + ex-net-cont-fee
           compute aa-net-cso-admin-fee (1 clas-indexa) =
              aa-net-cso-admin-fee (1 clas-indexa) +
              ex-net-cso-admin-fee
           compute aa-net-ga-sales (1 clas-indexa) =
              aa-net-ga-sales (1 clas-indexa) + ex-net-ga-sales
           compute aa-claim-pmts (1 clas-indexa) =
              aa-claim-pmts (1 clas-indexa) + ex-claim-pmts

           .
       0050-continue.

           perform 0020-read-input     thru 0020-exit

           .
       0050-exit.
           exit.

       0100-key-break.

           evaluate true
              when ws-pk-carrier not = ws-ck-carrier
                 perform 0200-account-break
                                       thru 0200-exit
                 perform 0300-state-break
                                       thru 0300-exit
                 perform 0400-grouping-break
                                       thru 0400-exit
                 perform 0500-carrier-break
                                       thru 0500-exit
              when ws-pk-group not = ws-ck-group
                 perform 0200-account-break
                                       thru 0200-exit
                 perform 0300-state-break
                                       thru 0300-exit
                 perform 0400-grouping-break
                                       thru 0400-exit
              when ws-pk-clp-state not = ws-ck-clp-state
                 perform 0200-account-break
                                       thru 0200-exit
                 perform 0300-state-break
                                       thru 0300-exit
              when ws-pk-account not = ws-ck-account
                 perform 0200-account-break
                                       thru 0200-exit
              when other
                 display ' something went wrong in 0100 '
                 perform abend-pgm
           end-evaluate

           .
       0100-exit.
           exit.

       0200-account-break.

           perform 0430-get-acct-name  thru 0430-exit

           if state-sub (clas-indexs) = ws-pk-clp-state
              continue
           else
              perform varying clas-indexs from clas-starts by +1 until
                 (state-sub (clas-indexs) = ws-pk-clp-state)
                 or (clas-indexs > clas-maxs)
              end-perform
              if clas-indexs <= clas-maxs
                 move state-pic (clas-indexs)
                                       to hd-st-nm
              end-if
           end-if

           move ws-pk-carrier          to hd-carr
           move ws-pk-group            to hd-group
           move ws-pk-clp-state        to hd-st
           move ws-pk-account          to hd-acct
           move acct-hd2               to sub-hd2
           move acct-hd3               to sub-hd3
           move +60 to lnctr
           move ' ' to lf-stuff ah-stuff
           move zeros                  to lf-net-clp
                                          lf-net-ow
                                          lf-net-pgm-mgt-fee
                                          lf-net-incentive
                                          lf-net-cont-fee
                                          lf-net-cso-admin-fee
                                          lf-net-ga-sales
                                          lf-claim-pmts
                                          ah-net-clp
                                          ah-net-ow
                                          ah-net-pgm-mgt-fee
                                          ah-net-incentive
                                          ah-net-cont-fee
                                          ah-net-cso-admin-fee
                                          ah-net-ga-sales
                                          ah-claim-pmts

           move +1 to r1
           perform 0900-process        thru 0900-exit
           move +1 to r1    *>  Account level totals
           move +2 to r2    *>  State   level totals
           perform 0700-roll-totals    thru 0700-exit
           move ws-init-accums         to ws-accums (r1)

           .
       0200-exit.
           exit.

       0300-state-break.

           move spaces                 to save-name
           move ws-pk-carrier          to hd-carr
           move ws-pk-group            to hd-group
           move ws-pk-clp-state        to hd-st
           move ws-pk-account          to hd-acct
           move st-hd2                 to sub-hd2
           move st-hd3                 to sub-hd3
           move +60 to lnctr
           move ' ' to lf-stuff ah-stuff
           move zeros                  to lf-net-clp
                                          lf-net-ow
                                          lf-net-pgm-mgt-fee
                                          lf-net-incentive
                                          lf-net-cont-fee
                                          lf-net-cso-admin-fee
                                          lf-net-ga-sales
                                          lf-claim-pmts
                                          ah-net-clp
                                          ah-net-ow
                                          ah-net-pgm-mgt-fee
                                          ah-net-incentive
                                          ah-net-cont-fee
                                          ah-net-cso-admin-fee
                                          ah-net-ga-sales
                                          ah-claim-pmts

           move +2 to r1
           perform 0900-process        thru 0900-exit
           move +2 to r1    *>  State   level totals
           move +3 to r2    *>  Group   level totals
           perform 0700-roll-totals    thru 0700-exit
           move ws-init-accums         to ws-accums (r1)

           .
       0300-exit.
           exit.

       0400-grouping-break.

           move spaces                 to save-name
           move ws-pk-carrier          to hd-carr
           move ws-pk-group            to hd-group
           move ws-pk-clp-state        to hd-st
           move ws-pk-account          to hd-acct
           move co-hd2                 to sub-hd2
           move co-hd3                 to sub-hd3
           move +60 to lnctr
           move ' ' to lf-stuff ah-stuff
           move zeros                  to lf-net-clp
                                          lf-net-ow
                                          lf-net-pgm-mgt-fee
                                          lf-net-incentive
                                          lf-net-cont-fee
                                          lf-net-cso-admin-fee
                                          lf-net-ga-sales
                                          lf-claim-pmts
                                          ah-net-clp
                                          ah-net-ow
                                          ah-net-pgm-mgt-fee
                                          ah-net-incentive
                                          ah-net-cont-fee
                                          ah-net-cso-admin-fee
                                          ah-net-ga-sales
                                          ah-claim-pmts

           move +3 to r1
           perform 0900-process        thru 0900-exit
           move +3 to r1    *>  Group   level totals
           move +4 to r2    *>  Carrier level totals
           perform 0700-roll-totals    thru 0700-exit
           move ws-init-accums         to ws-accums (r1)

           .
       0400-exit.
           exit.

       0500-carrier-break.

           move spaces                 to save-name
           move ws-pk-carrier          to hd-carr
           move ws-pk-group            to hd-group
           move ws-pk-clp-state        to hd-st
           move ws-pk-account          to hd-acct
           MOVE 'Carrier Summary'      TO SUB-HD2
           move carr-hd3               to sub-hd3
           move +60 to lnctr
           move ' ' to lf-stuff ah-stuff
           move zeros                  to lf-net-clp
                                          lf-net-ow
                                          lf-net-pgm-mgt-fee
                                          lf-net-incentive
                                          lf-net-cont-fee
                                          lf-net-cso-admin-fee
                                          lf-net-ga-sales
                                          lf-claim-pmts
                                          ah-net-clp
                                          ah-net-ow
                                          ah-net-pgm-mgt-fee
                                          ah-net-incentive
                                          ah-net-cont-fee
                                          ah-net-cso-admin-fee
                                          ah-net-ga-sales
                                          ah-claim-pmts

           move +4 to r1
           perform 0900-process        thru 0900-exit
           move +4 to r1    *>  Carrier level totals
           move +5 to r2    *>  Final   level totals
           perform 0700-roll-totals    thru 0700-exit
           move ws-init-accums         to ws-accums (r1)

           .
       0500-exit.
           exit.

       0600-final-totals.

           move spaces                 to save-name
           move spaces                 to sub-hd3
           move 'Final Totals '        to sub-hd2
           move +60 to lnctr
           move ' ' to lf-stuff ah-stuff
           move zeros                  to lf-net-clp
                                          lf-net-ow
                                          lf-net-pgm-mgt-fee
                                          lf-net-incentive
                                          lf-net-cont-fee
                                          lf-net-cso-admin-fee
                                          lf-net-ga-sales
                                          lf-claim-pmts
                                          ah-net-clp
                                          ah-net-ow
                                          ah-net-pgm-mgt-fee
                                          ah-net-incentive
                                          ah-net-cont-fee
                                          ah-net-cso-admin-fee
                                          ah-net-ga-sales
                                          ah-claim-pmts

           move +5 to r1
           perform 0900-process        thru 0900-exit

           .
       0600-exit.
           exit.

       0700-roll-totals.

           perform varying clas-indexl from clas-startl by +1 until
              clas-indexl > clas-maxl
              compute aa-net-clp (r2 clas-indexl) =
                 aa-net-clp (r2 clas-indexl) +
                 aa-net-clp (r1 clas-indexl)
              compute aa-net-ow (r2 clas-indexl) =
                 aa-net-ow (r2 clas-indexl) +
                 aa-net-ow (r1 clas-indexl)
              compute aa-net-pgm-mgt-fee (r2 clas-indexl) =
                 aa-net-pgm-mgt-fee (r2 clas-indexl) +
                 aa-net-pgm-mgt-fee (r1 clas-indexl)
              compute aa-net-incentive (r2 clas-indexl) =
                 aa-net-incentive (r2 clas-indexl) +
                 aa-net-incentive (r1 clas-indexl)
              compute aa-net-cont-fee (r2 clas-indexl) =
                 aa-net-cont-fee (r2 clas-indexl) +
                 aa-net-cont-fee (r1 clas-indexl)
              compute aa-net-cso-admin-fee (r2 clas-indexl) =
                 aa-net-cso-admin-fee (r2 clas-indexl) +
                 aa-net-cso-admin-fee (r1 clas-indexl)
              compute aa-net-ga-sales (r2 clas-indexl) =
                 aa-net-ga-sales (r2 clas-indexl) +
                 aa-net-ga-sales (r1 clas-indexl)
              compute aa-claim-pmts (r2 clas-indexl) =
                 aa-claim-pmts (r2 clas-indexl) +
                 aa-claim-pmts (r1 clas-indexl)
           end-perform

           perform varying clas-indexa from clas-starta by +1 until
              clas-indexa > clas-maxa
              compute aa-net-clp (r2 clas-indexa) =
                 aa-net-clp (r2 clas-indexa) +
                 aa-net-clp (r1 clas-indexa)
              compute aa-net-ow (r2 clas-indexa) =
                 aa-net-ow (r2 clas-indexa) +
                 aa-net-ow (r1 clas-indexa)
              compute aa-net-pgm-mgt-fee (r2 clas-indexa) =
                 aa-net-pgm-mgt-fee (r2 clas-indexa) +
                 aa-net-pgm-mgt-fee (r1 clas-indexa)
              compute aa-net-incentive (r2 clas-indexa) =
                 aa-net-incentive (r2 clas-indexa) +
                 aa-net-incentive (r1 clas-indexa)
              compute aa-net-cont-fee (r2 clas-indexa) =
                 aa-net-cont-fee (r2 clas-indexa) +
                 aa-net-cont-fee (r1 clas-indexa)
              compute aa-net-cso-admin-fee (r2 clas-indexa) =
                 aa-net-cso-admin-fee (r2 clas-indexa) +
                 aa-net-cso-admin-fee (r1 clas-indexa)
              compute aa-net-ga-sales (r2 clas-indexa) =
                 aa-net-ga-sales (r2 clas-indexa) +
                 aa-net-ga-sales (r1 clas-indexa)
              compute aa-claim-pmts (r2 clas-indexa) =
                 aa-claim-pmts (r2 clas-indexa) +
                 aa-claim-pmts (r1 clas-indexa)
           end-perform

           .
       0700-exit.
           exit.

       0900-process.

           perform varying clas-indexl from clas-startl by +1 until
              (clas-indexl > clas-maxl)
              if (aa-net-clp (r1 clas-indexl) not = zeros)
                 or (aa-net-ow (r1 clas-indexl) not = zeros)
                 or (aa-net-pgm-mgt-fee (r1 clas-indexl) not = zeros)
                 or (aa-net-incentive (r1 clas-indexl) not = zeros)
                 or (aa-net-cont-fee (r1 clas-indexl) not = zeros)
                 or (aa-net-cso-admin-fee (r1 clas-indexl) not = zeros)
                 or (aa-net-ga-sales (r1 clas-indexl) not = zeros)
                 or (aa-claim-pmts (r1 clas-indexl) not = zeros)
                 set there-is-lf-stuff to true
                 move aa-ben-code (r1 clas-indexl)
                                       to p-type
                 move aa-ben-description (r1 clas-indexl)
                                       to p-desc
                 move aa-net-clp  (r1 clas-indexl)
                                       to p-net-clp
                 compute lf-net-clp =
                    lf-net-clp + aa-net-clp (r1 clas-indexl)
                 move aa-net-ow   (r1 clas-indexl)
                                       to p-over
                 compute lf-net-ow =
                    lf-net-ow + aa-net-ow (r1 clas-indexl)
                 move aa-net-pgm-mgt-fee (r1 clas-indexl)
                                       to p-pgm-mgt-fee
                 compute lf-net-pgm-mgt-fee =
                    lf-net-pgm-mgt-fee +
                    aa-net-pgm-mgt-fee (r1 clas-indexl)
                 move aa-net-incentive (r1 clas-indexl)
                                       to p-dlr-inc
                 compute lf-net-incentive =
                    lf-net-incentive +
                    aa-net-incentive (r1 clas-indexl)
                 move aa-net-cont-fee (r1 clas-indexl)
                                       to p-bank-fee
                 compute lf-net-cont-fee =
                    lf-net-cont-fee +
                    aa-net-cont-fee (r1 clas-indexl)
                 move aa-net-cso-admin-fee (r1 clas-indexl)
                                       to p-cso-admin
                 compute lf-net-cso-admin-fee =
                    lf-net-cso-admin-fee +
                    aa-net-cso-admin-fee (r1 clas-indexl)
                 move aa-net-ga-sales (r1 clas-indexl)
                                       to p-ga-sales
                 compute lf-net-ga-sales =
                    lf-net-ga-sales +
                    aa-net-ga-sales (r1 clas-indexl)
                 move aa-claim-pmts (r1 clas-indexl)
                                       to p-claim
                 compute lf-claim-pmts =
                    lf-claim-pmts + aa-claim-pmts (r1 clas-indexl)
                 perform 1000-print-a-line
                                       thru 1000-exit
              end-if
           end-perform

           if there-is-lf-stuff
              perform 1010-print-lf-total
                                       thru 1010-exit
           end-if

           perform varying clas-indexa from clas-starta by +1 until
              (clas-indexa > clas-maxa)
              if (aa-net-clp (r1 clas-indexa) not = zeros)
                 or (aa-net-ow (r1 clas-indexa) not = zeros)
                 or (aa-net-pgm-mgt-fee (r1 clas-indexa) not = zeros)
                 or (aa-net-incentive (r1 clas-indexa) not = zeros)
                 or (aa-net-cont-fee (r1 clas-indexa) not = zeros)
                 or (aa-net-cso-admin-fee (r1 clas-indexa) not = zeros)
                 or (aa-net-ga-sales (r1 clas-indexa) not = zeros)
                 or (aa-claim-pmts (r1 clas-indexa) not = zeros)
                 set there-is-ah-stuff to true
                 move aa-ben-code (r1 clas-indexa)
                                       to p-type
                 move aa-ben-description (r1 clas-indexa)
                                       to p-desc
                 move aa-net-clp  (r1 clas-indexa)
                                       to p-net-clp
                 compute ah-net-clp =
                    ah-net-clp + aa-net-clp (r1 clas-indexa)
                 move aa-net-ow   (r1 clas-indexa)
                                       to p-over
                 compute ah-net-ow =
                    ah-net-ow + aa-net-ow (r1 clas-indexa)
                 move aa-net-pgm-mgt-fee (r1 clas-indexa)
                                       to p-pgm-mgt-fee
                 compute ah-net-pgm-mgt-fee =
                    ah-net-pgm-mgt-fee +
                    aa-net-pgm-mgt-fee (r1 clas-indexa)
                 move aa-net-incentive (r1 clas-indexa)
                                       to p-dlr-inc
                 compute ah-net-incentive =
                    ah-net-incentive +
                    aa-net-incentive (r1 clas-indexa)
                 move aa-net-cont-fee (r1 clas-indexa)
                                       to p-bank-fee
                 compute ah-net-cont-fee =
                    ah-net-cont-fee +
                    aa-net-cont-fee (r1 clas-indexa)
                 move aa-net-cso-admin-fee (r1 clas-indexa)
                                       to p-cso-admin
                 compute ah-net-cso-admin-fee =
                    ah-net-cso-admin-fee +
                    aa-net-cso-admin-fee (r1 clas-indexa)
                 move aa-net-ga-sales (r1 clas-indexa)
                                       to p-ga-sales
                 compute ah-net-ga-sales =
                    ah-net-ga-sales + aa-net-ga-sales (r1 clas-indexa)
                 move aa-claim-pmts (r1 clas-indexa)
                                       to p-claim
                 compute ah-claim-pmts =
                    ah-claim-pmts + aa-claim-pmts (r1 clas-indexa)
                 perform 1000-print-a-line
                                       thru 1000-exit
              end-if
           end-perform

           if there-is-ah-stuff
              perform 1020-print-ah-total
                                       thru 1020-exit
           end-if

           if there-is-lf-stuff or there-is-ah-stuff
              perform 1030-print-lf-ah-total thru 1030-exit
           end-if

           .
       0900-exit.
           exit.

       1000-print-a-line.

           IF LNCTR > +058
              move p-rec               to save-p-rec
              PERFORM 0750-HD-RTN      THRU 0760-HD-XIT                     
      *       MOVE HD4                 TO P-LN                         
      *       MOVE SPACE-3             TO P-CCSW                       
      *       PERFORM 0770-PRT-RTN     THRU 0790-PRT-XIT                   
              MOVE HD5                 TO P-LN                         
              MOVE SPACE-2             TO P-CCSW                       
              PERFORM 0770-PRT-RTN     THRU 0790-PRT-XIT                   
              MOVE SPACE-2             TO P-CCSW
              move save-p-rec          to p-rec
           end-if

           PERFORM 0770-PRT-RTN        THRU 0790-PRT-XIT

           .
       1000-exit.
           exit.

       1010-print-lf-total.

           move '   '                  to p-type
           move 'TOTAL LF'             to p-desc
           move lf-net-clp             to p-net-clp
           move lf-net-ow              to p-over
           move lf-net-pgm-mgt-fee     to p-pgm-mgt-fee
           move lf-net-incentive       to p-dlr-inc
           move lf-net-cont-fee        to p-bank-fee
           move lf-net-cso-admin-fee   to p-cso-admin
           move lf-net-ga-sales        to p-ga-sales
           move lf-claim-pmts          to p-claim

           perform 1000-print-a-line   thru 1000-exit
           move spaces                 to p-rec
           perform 1000-print-a-line   thru 1000-exit

           .
       1010-exit.
           exit.

       1020-print-ah-total.

           move '   '                  to p-type
           move 'TOTAL AH'             to p-desc
           move ah-net-clp             to p-net-clp
           move ah-net-ow              to p-over
           move ah-net-pgm-mgt-fee     to p-pgm-mgt-fee
           move ah-net-incentive       to p-dlr-inc
           move ah-net-cont-fee        to p-bank-fee
           move ah-net-cso-admin-fee   to p-cso-admin
           move ah-net-ga-sales        to p-ga-sales
           move ah-claim-pmts          to p-claim

           perform 1000-print-a-line   thru 1000-exit
           move spaces                 to p-rec
           perform 1000-print-a-line   thru 1000-exit

           .
       1020-exit.
           exit.

       1030-print-lf-ah-total.

           move '   '                  to p-type
           move '     TOTAL'           to p-desc
           compute p-net-clp =
              lf-net-clp + ah-net-clp
           compute p-over =
              lf-net-ow + ah-net-ow
           compute p-pgm-mgt-fee =
              lf-net-pgm-mgt-fee + ah-net-pgm-mgt-fee
           compute p-dlr-inc =
              lf-net-incentive + ah-net-incentive
           compute p-bank-fee =
              lf-net-cont-fee + ah-net-cont-fee
           compute p-cso-admin =
              lf-net-cso-admin-fee + ah-net-cso-admin-fee
           compute p-ga-sales =
              lf-net-ga-sales + ah-net-ga-sales
           compute p-claim =
              lf-claim-pmts + ah-claim-pmts

           perform 1000-print-a-line   thru 1000-exit

           .
       1030-exit.
           exit.
                                                                        
                                                                        
       0170-CLOSE-FILES.                                                

           CLOSE detail-in prntr eracctt

           .
       0170-exit.
           EXIT.                                                        
                                                                        
       0430-get-acct-name.

           MOVE DTE-CLASIC-COMPANY-CD  TO am-control-primary
           move ws-pk-carrier          to am-carrier
           move ws-pk-group            to am-grouping
           move ws-prev-acct-state     to am-state
           move ws-pk-account          to am-account
           move zeros                  to am-expire-dt
           move am-control-a           to ws-compare-key
           move 'UNKNOWN'              to save-name

           display ' about to start account ' ws-compare-key

           start eracctt key >= am-control-primary
           if eracct-file-status = '00'
              read eracctt next record
              if eracct-file-status = '00'
                 perform until
                    (eracct-file-status not = '00')
                    or (am-control-a not = ws-compare-key)
                    move am-name       to save-name
                    read eracctt next record
                 end-perform
              else
                 display ' bad read eracctt ' eracct-file-status ' '
                    am-state ' ' am-account
              end-if
           else
                 display ' bad start eracctt ' eracct-file-status ' '
                    am-state ' ' am-account
           end-if

           .
       0430-exit.
           exit.

       0750-HD-RTN.                                                     
           ADD +1                      TO  PGCTR.                       
           MOVE PGCTR                  TO  HD-PAGE.                     
           MOVE HD1                    TO  P-LN.                        
           MOVE SPACE-N                TO  P-CCSW.                      
           PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      
           MOVE HD2                    TO  P-LN.                        
           PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      
           MOVE HD3                    TO  P-LN.                        
           PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      
           MOVE SAVE-NAME              TO  P-LN.                        
           PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      
           MOVE +5                     TO  LNCTR.                       
                                                                        
       0760-HD-XIT.                                                     
           EXIT.                                                        
                                                                        
       0770-PRT-RTN.                                                    
           MOVE P-CCSW                 TO  X P-CTL.                     
           MOVE P-LN                   TO  P-DATA.                      
           MOVE SPACE-1                TO  P-REC.                       
                                                                        
           IF X = SPACE-1                                               
               ADD 1                   TO  LNCTR                        
           ELSE                                                         
               IF X = SPACE-2                                           
                   ADD 2               TO  LNCTR                        
               ELSE                                                     
                   IF X = SPACE-3                                       
                       ADD 3           TO  LNCTR.                       
                                                                        
       0780-PRT-COPY.                                                   

           IF (DTE-FICH <> SPACE)
              and (FICH-OPEN = SPACE)
              MOVE 'X'                 TO FICH-OPEN
              OPEN OUTPUT FICH
           end-if
                                                     
           IF (DTE-FICH <> SPACE)
              MOVE X                   TO P-CTL
              WRITE FICH-REC FROM PRT
           end-if

           IF SUB-HD2 = 'Final Totals' OR 'Carrier Summary'
              CONTINUE
           ELSE
              GO TO 0790-PRT-XIT
           END-IF


           IF DTE-FICH = SPACE OR '2'               
             MOVE X                    TO  P-CTL    
             IF P-CTL = ' '                         
               WRITE PRT AFTER ADVANCING 1 LINE     
             ELSE                                   
               IF P-CTL = '0'                       
                 WRITE PRT AFTER ADVANCING 2 LINES  
               ELSE                                 
                 IF P-CTL = '-'                     
                   WRITE PRT AFTER ADVANCING 3 LINES
                 ELSE                               
                   WRITE PRT AFTER ADVANCING PAGE.  

           .
       0790-PRT-XIT.                                                    
           EXIT.                                                        

       ABEND-PGM.                                                       
                                       COPY ELCABEND SUPPRESS.          

       LCP-WRITE-POS-PRT SECTION.                                       
           IF LCP-ASA = '+'                                             
               WRITE PRT AFTER 0 LINE                                   
           ELSE                                                         
           IF LCP-ASA = ' '                                             
               WRITE PRT AFTER ADVANCING 1 LINE                         
           ELSE                                                         
           IF LCP-ASA = '0'                                             
               WRITE PRT AFTER ADVANCING 2 LINE                         
           ELSE                                                         
           IF LCP-ASA = '-'                                             
               WRITE PRT AFTER ADVANCING 3 LINE                         
           ELSE                                                         
           IF LCP-ASA = '1'                                             
               WRITE PRT AFTER ADVANCING PAGE                           
           ELSE                                                         
           IF LCP-ASA = '2'                                             
               WRITE PRT AFTER ADVANCING LCP-CH2                        
           ELSE                                                         
           IF LCP-ASA = '3'                                             
               WRITE PRT AFTER ADVANCING LCP-CH3                        
           ELSE                                                         
           IF LCP-ASA = '4'                                             
               WRITE PRT AFTER ADVANCING LCP-CH4                        
           ELSE                                                         
           IF LCP-ASA = '5'                                             
               WRITE PRT AFTER ADVANCING LCP-CH5                        
           ELSE                                                         
           IF LCP-ASA = '6'                                             
               WRITE PRT AFTER ADVANCING LCP-CH6                        
           ELSE                                                         
           IF LCP-ASA = '7'                                             
               WRITE PRT AFTER ADVANCING LCP-CH7                        
           ELSE                                                         
           IF LCP-ASA = '8'                                             
               WRITE PRT AFTER ADVANCING LCP-CH8                        
           ELSE                                                         
           IF LCP-ASA = '9'                                             
               WRITE PRT AFTER ADVANCING LCP-CH9                        
           ELSE                                                         
           IF LCP-ASA = 'A'                                             
               WRITE PRT AFTER ADVANCING LCP-CH10                       
           ELSE                                                         
           IF LCP-ASA = 'B'                                             
               WRITE PRT AFTER ADVANCING LCP-CH11                       
           ELSE                                                         
           IF LCP-ASA = 'C'                                             
               WRITE PRT AFTER ADVANCING LCP-CH12                       
           ELSE                                                         
           IF LCP-ASA = 'V'                                             
               WRITE PRT AFTER ADVANCING LCP-P01                        
           ELSE                                                         
           IF LCP-ASA = 'W'                                             
               WRITE PRT AFTER ADVANCING LCP-P02                        
           ELSE                                                         
           DISPLAY 'ASA CODE ERROR'.                                    
       LCP-WRITE-END-PRT.                                               
           EXIT.                                                        
