      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID.                 EL1504.                              
      *                                                                 
      *AUTHOR.     Cowtown
      *            OMAHA, NE.
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.    TRANSACTION - E024 - CLAIM PAYMENT HISTORY
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 021615    2014062700002  PEMA  NEW PROGRAM
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
051215* 051215  IR2015051100002  PEMA  ADD CERT SUFFIX TO MAP
060315* 060315  IR2015060200004  PEMA  Correct cut-off date
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
111816* 111816  IR2016111500001  PEMA  CORRECT INT CHK CASHED DT PROCESS
013017* 013017  CR2017022000001  PEMA  DRAFTS TO CHECKS - DCC
062717* 062717  IR2017062600001  PEMA  Correct DCC cut-off date.
082218* 082218  CR2018051400001  TANA  Hold and Pay
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL1504 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
       77  s3                          pic s999 comp-3 value +0.
       77  p1                          pic s999 comp-3 value +0.
       77  WS-HOLD-KEY                 PIC X(20).
       77  WS-HOLD-S1                  PIC S999 COMP-3 VALUE +0.
       77  ws-prev-clm-type            pic x   value ' '.
       77  ws-prev-ins-type            pic x   value ' '.
       77  ws-prev-ben-per             pic 99 value zeros.
       77  WS-CNTR                     pic s999 comp-3 value +0.
       77  ws-map-line-no              pic 99  value zeros.
       77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
       77  ws-accum-days               pic s9(5) comp-3 value +0.
       77  ws-accum-amt                pic s9(9)v99 comp-3 value +0.
       77  ws-accum-pd-bens            pic s999 comp-3 value +0.
       77  wk1                         pic 999 value zeros.
       77  wk2                         pic 999 value zeros.
       77  ws-eracct-sw                pic x  value spaces.
           88  acct-found               value 'Y'.
       77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
       77  ws-connect-db-sw            pic x value ' '.
           88  connected-to-db            value 'Y'.
       77  ws-line-no                  pic 99 value zeros.
       77  w-row                       pic s999 value +0.
       77  w-column                    pic s999 value +0.
       77  ws-remainder                pic 9.
       77  ws-sort-by-sw               pic x.
           88  sort-by-pd-dt              value '1'.
           88  sort-by-pay-from           value '2'.
           88  sort-by-pay-thru           value '3'.
           88  sort-by-cash-dt            value '4'.

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
      
       01  SQLCMD                      PIC X(1024).
       01  SVR                         PIC X(32).
       01  USR                         PIC X(32).
       01  PASS                        PIC X(32).
       01  USR-PASS                    PIC X(64).
      
       01  WS-SQL-DATA.
111714     05  ws-draft-or-check       pic x.
           05  WS-CHECK-TYPE           PIC X.
111714*    05  WS-CHECK-NUMBER         PIC X(7).
111714     05  WS-CHECK-NUMBER         PIC X(10).
111714     05  ws-claim-number         pic x(7).
           05  WS-CHECK-AMOUNT         PIC X(10).
111714     05  WS-CHECK-COMPANY        PIC X(5).
111714     05  WS-CHECK-CASHED-DT      PIC X(20).
      
       01  ws-ach-data.
           05  ws-carrier              pic x.
           05  ws-state                pic xx.
           05  ws-account-no           pic x(10).
           05  ws-cert-no              pic x(11).
           05  ws-claim-no             pic x(7).
           05  ws-ach-check-no         pic x(10).
           05  ws-cashed-date          pic x(10).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC
      
                                           COPY ELCSCTM.                
                                                                        
                                           COPY ELCSCRTY.               
                                                                        
       01  WS-DATE-AREA.                                                
           12  SAVE-DATE                   PIC X(8)    VALUE SPACES.    
           12  SAVE-DATE-CCYYMMDD.                                      
               16  SAVE-DATE-CC            PIC XX      VALUE SPACES.    
               16  SAVE-DATE-YMD.                                       
                   20  SAVE-DATE-YY        PIC XX      VALUE SPACES.    
                   20  FILLER              PIC X(4)    VALUE SPACES.    
           12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    
                                                                        
       01  output-screen-work-area.
           05  os-prev-key.
               10  os-prev-clm-type        pic x.
               10  os-prev-ins-type        pic x.
           05  ws-pd-bens                  pic 999 value zeros.
           05  ws-cov-rem-bens             pic s999 value zeros.
      
       01  STANDARD-AREAS.                                              
           12  GETMAIN-SPACE               PIC X       VALUE SPACE.     
           12  MAP-NAME                    PIC X(8)    VALUE 'EL150E'.  
           12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1504S'. 
           12  TRANS-ID                    PIC X(4)    VALUE 'E024'.    
           12  THIS-PGM                    PIC X(8)    VALUE 'EL1504'.  
           12  PGM-NAME                    PIC X(8).                    
           12  TIME-IN                     PIC S9(7).                   
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER                  PIC X.                       
               16  TIME-OUT                PIC 99V99.                   
               16  FILLER                  PIC XX.                      
           12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   
           12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   
           12  XCTL-126                    PIC X(8)    VALUE 'EL126'.   
           12  xctl-150                    pic x(8)    value 'EL150'.
           12  XCTL-142                    PIC X(8)    VALUE 'EL142'.   
           12  LINK-001                    PIC X(8)    VALUE 'EL001'.   
           12  LINK-004                    PIC X(8)    VALUE 'EL004'.   
           12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. 
           12  FILE-ID                     PIC X(8).                    
           12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
           12  WS-ITEM-COUNT               PIC S9(4)   VALUE +1    COMP.
      
       01  MISC-WORK-AREAS.                                             
      
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMIDERR           VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-DUPREC              VALUE +14.
               88  RESP-DUPKEY              VALUE +15.
               88  RESP-INVREQ              VALUE +16.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-ILLOGIC             VALUE +21.
               88  RESP-LENGERR             VALUE +22.
      
           12  W-CALLING-PGM               PIC X(8).                    
           12  QID.                                                     
               16  QID-TERM                PIC X(4).                    
               16  FILLER                  PIC X(4)    VALUE '150E'.
           12  WS-TABLE-QID.                                            
               16  WS-QID-TERM             PIC X(4).                    
               16  FILLER                  PIC X(4)    VALUE 'TBLE'.    
                                                                        
           12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.
           12  WS-TABLE-LENGTH             PIC S9(4)   VALUE +8400 COMP.
           12  PASS-SWITCH                 PIC X       VALUE 'A'.       
           12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.
           12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.    
           12  WS-SUB                      PIC 9       VALUE 0.         
           12  SUB                         PIC 9       VALUE 1.         
           12  SUB-1                       PIC 9       VALUE 1.         
           12  RETURNED-FROM               PIC X(8)    VALUE SPACES.    
           12  DIRECTION-SWITCH            PIC X       VALUE 'N'.       
           12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.       
               88  RECORDS-READ                        VALUE 'Y'.       
           12  WS-ENDBR-SW                 PIC X       VALUE 'N'.       
           12  SAVE-CONTROL                PIC X(39).                   
           12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.
           12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.     
           12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD             
                                           PIC S9(16).                  
                                                                        
           12  WS-UPDATE-SW                PIC X       VALUE 'N'.       
                                                                        
           12  WS-WORK-DATE.                                            
               16  WS-WORK-MM              PIC 99      VALUE ZEROS.     
               16  WS-WORK-DD              PIC 99      VALUE ZEROS.     
               16  WS-WORK-YY              PIC 99      VALUE ZEROS.     
                                                                        
                                                                        
           12  HOLD-SUB                    PIC 9(4)    VALUE ZEROS.     
           12  SUB-2                       PIC 9(4)    VALUE ZEROS.     
           12  SUB-3                       PIC 9(4)    VALUE ZEROS.     
           12  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.     
      

00159  01  WS-UNSORTED-TABLE.  *> 56 * 150 = 8400
00160      12  WS-UNSRTD-TABLE   OCCURS 150 TIMES.
               16  ws-key.
                   20  ws-key-dt       pic xx.
               16  ws-check-no         pic x(7).
               16  ws-pd-from-dt       pic xx.
               16  ws-pd-thru-dt       pic xx.
               16  ws-pd-dt            pic xx.
               16  ws-cashed-dt        pic xx.
               16  ws-void-dt          pic xx.
               16  ws-check-amt        pic s9(7)v99 comp-3.
               16  ws-pmt-type         pic x.
               16  ws-payee            pic x(30).
               16  ws-sorted-sw        pic x.

00166  01  WS-SORTED-TABLE.                                             
00167      12  WS-SRTD-TABLE OCCURS 150 TIMES.
               16  ws-srtd-key.
                   20  ws-srtd-key-dt  pic xx.
               16  ws-srtd-check-no    pic x(7).
               16  ws-srtd-pd-from-dt  pic xx.
               16  ws-srtd-pd-thru-dt  pic xx.
               16  ws-srtd-pd-dt       pic xx.
               16  ws-srtd-cashed-dt   pic xx.
               16  ws-srtd-void-dt     pic xx.
               16  ws-srtd-check-amt   pic s9(7)v99 comp-3.
               16  ws-srtd-pmt-type    pic x.
               16  ws-srtd-payee       pic x(30).
               16  ws-srtd-sw          pic x.

       01  work-line-1.
           05  w-line-1-no             pic 99.
           05  filler                  pic xx.
           05  w-line-1-check-no       pic x(10).
           05  w-line-1-pd-from        pic x(9).
           05  w-line-1-pd-thru        pic x(9).
           05  w-line-1-pd-dt          pic x(9).
           05  w-line-1-cash-dt        pic x(9).
           05  w-line-1-void-dt        pic x(12).
           05  w-line-1-pd-amt         pic x(15).
           05  w-line-1-pmt-type       pic x.
           
       01  work-line-2.
           05  f                       pic x(13) value ' '.
           05  w-line-2-payee          pic x(30).

       01  filler.
           05  ws-pmt-amt-tmp              pic $,$$$,$$$.99-.
           05  ws-pmt-amt-tmpx redefines
               ws-pmt-amt-tmp              pic x(13).

           05  WS-CHECK-AMT-TMP            PIC z(7).99.
           05  WS-CHECK-AMT-TMPX REDEFINES 
               WS-CHECK-AMT-TMP            PIC X(10).
      
       01  ACCESS-KEYS.                                                 
           12  ERACCT-KEY.                                      
               16  ERACCT-PARTIAL-KEY.                          
                   20  ACCT-COMP-CD    PIC X.                   
                   20  ACCT-CARRIER    PIC X.                   
                   20  ACCT-GROUPING   PIC X(6).                
                   20  ACCT-STATE      PIC XX.                  
                   20  ACCT-ACCOUNT    PIC X(10).               
               16  ACCT-EXP-DT         PIC XX.                  
               16  acct-filler         PIC X(4) VALUE SPACES.   
           12  ELMSTR-KEY.                                              
               16  MSTR-COMP-CD            PIC X.                       
               16  MSTR-CARRIER            PIC X.                       
               16  MSTR-CLAIM-NO           PIC X(7).                    
               16  MSTR-CERT-NO.                                        
                   20  MSTR-CERT-NO-PRIME  PIC X(10).                   
                   20  MSTR-CERT-NO-SUFX   PIC X.                       
           12  ELTRLR-KEY.                                              
               16  TRLR-COMP-CD            PIC X.                       
               16  TRLR-CARRIER            PIC X.                       
               16  TRLR-CLAIM-NO           PIC X(7).                    
               16  TRLR-CERT-NO            PIC X(11).
               16  trlr-seq-no             pic s9(4) comp.
           12  ELCERT-KEY.                                              
               16  CERT-COMP-CD            PIC X.                       
               16  CERT-CARRIER            PIC X.                       
               16  CERT-GROUPING           PIC X(6).                    
               16  CERT-STATE              PIC XX.                      
               16  CERT-ACCOUNT            PIC X(10).                   
               16  CERT-EFF-DT             PIC XX.                      
               16  CERT-CERT-NO.                                        
                   20  CERT-CERT-NO-PRIME  PIC X(10).                   
                   20  CERT-CERT-NO-SUFX   PIC X.                       
           12  ELCRTT-KEY.                                              
               16  CRTT-COMP-CD            PIC X.                       
               16  CRTT-CARRIER            PIC X.                       
               16  CRTT-GROUPING           PIC X(6).                    
               16  CRTT-STATE              PIC XX.                      
               16  CRTT-ACCOUNT            PIC X(10).                   
               16  CRTT-EFF-DT             PIC XX.                      
               16  CRTT-CERT-NO            PIC X(11).                   
               16  CRTT-REC-TYPE           PIC X.
      
       01  ERROR-MESSAGES.                                              
           12  ER-0000                     PIC X(4)    VALUE '0000'.    
           12  ER-0004                     PIC X(4)    VALUE '0004'.    
           12  ER-0008                     PIC X(4)    VALUE '0008'.    
           12  ER-0029                     PIC X(4)    VALUE '0029'.    
           12  ER-0033                     PIC X(4)    VALUE '0033'.    
           12  ER-0042                     PIC X(4)    VALUE '0042'.    
           12  ER-0068                     PIC X(4)    VALUE '0068'.    
           12  ER-0070                     PIC X(4)    VALUE '0070'.    
           12  ER-0130                     PIC X(4)    VALUE '0130'.    
           12  ER-0154                     PIC X(4)    VALUE '0154'.    
           12  ER-0169                     PIC X(4)    VALUE '0169'.    
           12  ER-0172                     PIC X(4)    VALUE '0172'.    
           12  ER-0190                     PIC X(4)    VALUE '0190'.    
           12  ER-0204                     PIC X(4)    VALUE '0204'.    
           12  ER-0206                     PIC X(4)    VALUE '0206'.    
           12  ER-0282                     PIC X(4)    VALUE '0282'.    
           12  ER-0303                     PIC X(4)    VALUE '0303'.    
           12  ER-0334                     PIC X(4)    VALUE '0334'.    
           12  ER-0335                     PIC X(4)    VALUE '0335'.    
           12  ER-0336                     PIC X(4)    VALUE '0336'.    
           12  ER-0337                     PIC X(4)    VALUE '0337'.    
           12  ER-0338                     PIC X(4)    VALUE '0338'.    
           12  ER-0376                     PIC X(4)    VALUE '0376'.    
           12  ER-0412                     PIC X(4)    VALUE '0412'.    
           12  ER-0413                     PIC X(4)    VALUE '0413'.    
           12  ER-0660                     PIC X(4)    VALUE '0660'.    
           12  ER-0661                     PIC X(4)    VALUE '0661'.    
           12  ER-0662                     PIC X(4)    VALUE '0662'.    
           12  ER-0663                     PIC X(4)    VALUE '0663'.    
           12  ER-0664                     PIC X(4)    VALUE '0664'.    
           12  ER-0665                     PIC X(4)    VALUE '0665'.    
           12  ER-0666                     PIC X(4)    VALUE '0666'.    
           12  ER-0667                     PIC X(4)    VALUE '0667'.    
           12  ER-0672                     PIC X(4)    VALUE '0672'.    
           12  ER-0776                     PIC X(4)    VALUE '0776'.    
           12  ER-0800                     PIC X(4)    VALUE '0800'.    
           12  ER-0801                     PIC X(4)    VALUE '0801'.    
           12  ER-0816                     PIC X(4)    VALUE '0816'.    
           12  ER-0823                     PIC X(4)    VALUE '0823'.    
           12  ER-0833                     PIC X(4)    VALUE '0833'.    
           12  ER-0835                     PIC X(4)    VALUE '0835'.    
           12  ER-0849                     PIC X(4)    VALUE '0849'.    
           12  ER-0919                     PIC X(4)    VALUE '0919'.    
           12  ER-0920                     PIC X(4)    VALUE '0920'.    
           12  ER-0921                     PIC X(4)    VALUE '0921'.    
           12  ER-0922                     PIC X(4)    VALUE '0922'.    
           12  ER-0923                     PIC X(4)    VALUE '0923'.    
           12  ER-0925                     PIC X(4)    VALUE '0925'.    
           12  ER-0939                     PIC X(4)    VALUE '0939'.    
           12  ER-0940                     PIC X(4)    VALUE '0940'.    
           12  ER-0941                     PIC X(4)    VALUE '0941'.    
           12  ER-0942                     PIC X(4)    VALUE '0942'.    
           12  ER-0946                     PIC X(4)    VALUE '0946'.    
           12  ER-0947                     PIC X(4)    VALUE '0947'.    
           12  ER-0948                     PIC X(4)    VALUE '0948'.    
           12  ER-0949                     PIC X(4)    VALUE '0949'.    
           12  ER-0950                     PIC X(4)    VALUE '0950'.    
           12  ER-0951                     PIC X(4)    VALUE '0951'.    
           12  ER-0952                     PIC X(4)    VALUE '0952'.    
           12  ER-0954                     PIC X(4)    VALUE '0954'.    
           12  ER-0974                     PIC X(4)    VALUE '0974'.    
           12  ER-0975                     PIC X(4)    VALUE '0975'.    
           12  ER-2238                     PIC X(4)    VALUE '2238'.
           12  ER-2378                     PIC X(4)    VALUE '2378'.    
           12  ER-2379                     PIC X(4)    VALUE '2379'.    
           12  ER-7999                     PIC X(4)    VALUE '7999'.    
           12  ER-8003                     PIC X(4)    VALUE '8003'.    
           12  ER-8051                     PIC X(4)    VALUE '8051'.    
           12  ER-8052                     PIC X(4)    VALUE '8052'.    
           12  ER-8053                     PIC X(4)    VALUE '8053'.    
           12  ER-8054                     PIC X(4)    VALUE '8054'.    
           12  ER-8055                     PIC X(4)    VALUE '8055'.    
           12  ER-8056                     PIC X(4)    VALUE '8056'.    
           12  ER-8057                     PIC X(4)    VALUE '8057'.    
           12  ER-8058                     PIC X(4)    VALUE '8058'.    
           12  ER-8059                     PIC X(4)    VALUE '8059'.    
           12  ER-8060                     PIC X(4)    VALUE '8060'.    
           12  ER-8061                     PIC X(4)    VALUE '8061'.    
           12  ER-8062                     PIC X(4)    VALUE '8062'.    
           12  ER-8063                     PIC X(4)    VALUE '8063'.    
           12  ER-8064                     PIC X(4)    VALUE '8064'.    
           12  ER-8065                     PIC X(4)    VALUE '8065'.    
           12  ER-8066                     PIC X(4)    VALUE '8066'.    
           12  ER-8152                     PIC X(4)    VALUE '8152'.    
           12  ER-8153                     PIC X(4)    VALUE '8153'.    
           12  ER-8154                     PIC X(4)    VALUE '8154'.    
           12  ER-8155                     PIC X(4)    VALUE '8155'.    
           12  ER-9211                     PIC X(4)    VALUE '9211'.    
           12  ER-9883                     PIC X(4)    VALUE '9883'.    
      
       01  TEXT-WORK-AREAS.
           05  ws-cov-type             pic x(4) value spaces.
      
           05  M1                         PIC S999 COMP-3 VALUE +0.
      
                                       COPY ELCDATE.                
                                       COPY ELCLOGOF.               
                                       COPY ELCATTR.                
                                       COPY ELCEMIB.                
                                       COPY ELCINTF.                
      
           12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.              
      
               16  FILLER                  PIC x(2).                    
               16  pi-el142-priority       pic x.
               16  filler                  pic x.
               16  PI-MAP-NAME             PIC X(8).                    
               16  FILLER                  PIC X(8).                    
               16  PI-RECORD-COUNT              PIC S9        COMP-3.   
               16  PI-END-OF-FILE               PIC S9        COMP-3.   
               16  PI-SAVE-CURSOR               PIC S9(4)     COMP.     
               16  PI-PURGED-SW                 PIC X.                  
                   88  CLAIM-IS-PURGED                 VALUE 'Y'.       
               16  PI-LINE-NO                   PIC 9.                  
               16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.       
               16  PI-FIRST-TIME-SW             PIC X.                  
                   88  FIRST-TIME                      VALUE 'Y'.       
                                                                        
               
               16  PI-PREV-CLAIM-NO             PIC X(7).               
               16  PI-PREV-CARRIER              PIC X.                  
               16  PI-PRIME-CERT-NO.                                    
                   20  PI-PRIME-CERT            PIC X(10).              
                   20  PI-PRIME-SUFX            PIC X.                  
               16  PI-PRIME-HDG                 PIC X(12).              
               16  PI-MAX-SUB                   PIC 9(4).               
               16  PI-PAY-TYPE                  PIC X.                  
               16  PI-FULL-SCREEN-IND           PIC X.                  
                   88  PI-FULL-SCREEN-SHOWN         VALUE 'Y'.          
               16  pi-mo-ben              pic s9(9)v99 comp-3.
               16  pi-max-moben           pic s9(7)v99 comp-3.
               16  pi-exp-dt               pic xx.
               16  pi-last-seq-no          pic s9(4) comp.
               16  pi-first-seq-no         pic s9(4) comp.
               16  pi-last-line-no         pic 99.
               16  pi-first-line-no        pic 99.
               16  pi-map-borr             pic x(30).
               16  pi-map-total-pd         pic s9(9)v99 comp-3.
               16  pi-map-total-int        pic s9(9)v99 comp-3.
               16  pi-sub                  pic s999 comp-3.
               16  pi-recs-last-page       pic 9.
               16  FILLER                  PIC X(288).
                                                                        
           EJECT                                                        
                                           copy DFHBMSCA.
                                           COPY ELCAID.                 
       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER                      PIC X(8).                    
           12  PF-VALUES                   PIC X       OCCURS 24 TIMES. 
      
                                           COPY EL1504S.
      
       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 
      
                                           COPY ELCCNTL.                
                                           COPY ELCTRLR.
                                           COPY ELCMSTR.
                                           COPY ERCACCT.
      
       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE DC-GREG-DATE-1-YMD     TO SAVE-DATE-YMD
      
           IF SAVE-DATE-YY > 70
              MOVE 19                  TO SAVE-DATE-CC
           ELSE
              MOVE 20                  TO SAVE-DATE-CC
           END-IF
                                                                        
           IF EIBCALEN = 0                                              
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
      
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
      
           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
      
           MOVE EIBTRMID               TO QID-TERM
                                          WS-QID-TERM
      
           IF PI-RETURN-TO-PROGRAM = THIS-PGM                           
              MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM
           END-IF
      
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
              ELSE                                                     
                 MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                 MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                 MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                 MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                 MOVE SPACES               TO PI-SAVED-PROGRAM-6
              END-IF
           END-IF
      
           IF RETURNED-FROM NOT = SPACES                                
              go to 0100-first-time-in
           END-IF
      
           IF EIBAID = DFHCLEAR                                         
               GO TO 9400-CLEAR.                                        
                                                                        
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              CONTINUE
           ELSE                                                         
              EXEC CICS READQ TS                                       
                 QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
                 INTO    (SECURITY-CONTROL)                           
                 LENGTH  (SC-COMM-LENGTH)                             
                 ITEM    (SC-ITEM)                                    
              END-EXEC                                                 
              MOVE SC-CLAIMS-DISPLAY (16)
                                       TO PI-DISPLAY-CAP      
              MOVE SC-CLAIMS-UPDATE  (16)
                                       TO PI-MODIFY-CAP       
              IF NOT DISPLAY-CAP                                   
                 MOVE 'READ'           TO SM-READ             
                 PERFORM 9995-SECURITY-VIOLATION                  
                 MOVE ER-0070          TO EMI-ERROR           
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT         
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF
                                                                        
           IF EIBTRNID = TRANS-ID                                       
              GO TO 0200-RECEIVE
           END-IF
      
           .
       0100-FIRST-TIME-IN.                                              
           MOVE LOW-VALUES             TO EL150EO
                                          PI-PROGRAM-WORK-AREA
      
           MOVE 'Y'                    TO PI-FIRST-TIME-SW
                                          WS-RECORDS-READ-SW
           MOVE 'F'                    TO DIRECTION-SWITCH
           MOVE 1                      TO PI-LINE-NO                   
                                          SUB-2                        
                                          SUB-3
           move +1                     to m1
           MOVE ZERO                   TO WS-MAX-SUB                   
                                          SUB
                                          pi-last-seq-no
                                          pi-first-seq-no
                                          pi-max-moben
           move +1                     to pi-first-line-no
                                          ws-line-no
           move +6                     to pi-last-line-no

           PERFORM 7400-DEL-TEMP-STOR-TABLE
                                       THRU 7400-EXIT
      
           EXEC CICS DELETEQ TS                                         
              QUEUE  (QID)
              RESP   (WS-RESPONSE)
           END-EXEC
      
           perform 1000-build-init-table
                                       thru 1000-exit
           move high-values            to ws-key (s1)
           compute ws-max-sub = s1 - +1
           move ws-max-sub             to pi-max-sub
                                          ws-cntr
           perform 2000-desc-sort      thru 2000-exit
           move +1                     to pi-sub
           PERFORM 0900-browse-fwd     THRU 0900-EXIT
      
           GO TO 8100-SEND-INITIAL-MAP
      
           .
       0200-RECEIVE.                                                    
           MOVE 'B'                    TO  PASS-SWITCH.                 
           MOVE LOW-VALUES             TO  EL150EI.                     
                                                                        
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
               MOVE ER-0008            TO  EMI-ERROR                    
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                 TO  ENTERPFL                     
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           EXEC CICS RECEIVE                                            
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               INTO     (EL150EI)                                       
           END-EXEC.                                                    
                                                                        
           IF ENTERPFL = 0                                              
               GO TO 0300-CHECK-PFKEYS.                                 
                                                                        
           IF EIBAID NOT = DFHENTER                                     
               MOVE ER-0004            TO  EMI-ERROR                    
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
           IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)            
               MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   
           ELSE                                                         
               MOVE ER-0029                TO  EMI-ERROR                
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
       0300-CHECK-PFKEYS.                                               
           IF EIBAID = DFHPF23                                          
               GO TO 8810-PF23.                                         
                                                                        
           IF EIBAID = DFHPF24                                          
               GO TO 9200-RETURN-MAIN-MENU.                             
                                                                        
           IF EIBAID = DFHPF12                                          
               GO TO 9500-PF12.                                         
      
           MOVE SPACES                 TO ERRMSG1O
      
           IF EIBAID = DFHPF1                                           
              perform 7500-recover-temp-storage
                                       thru 7500-exit
              move +1                  to m1
              move pi-last-line-no     to ws-line-no
              MOVE 'F'                 TO DIRECTION-SWITCH             
              perform 0900-browse-fwd  thru 0900-exit
              GO TO 8100-SEND-INITIAL-MAP
           end-if
      
           IF EIBAID = DFHPF2                                           
              perform 7500-recover-temp-storage
                                       thru 7500-exit
              MOVE 'B'                 TO DIRECTION-SWITCH             
              perform 0910-browse-bwd  thru 0910-exit
              GO TO 8100-SEND-INITIAL-MAP
           end-if
      
           IF EIBAID = DFHENTER                                         
               GO TO 0330-EDIT-DATA.                                    
                                                                        
           divide eibcposn by 80 giving w-row remainder w-column
           add 1 to w-row
           move '1'                    to ws-sort-by-sw
           if eibaid = dfhpf7 or dfhpf8
              if w-row = 6
                 evaluate true
                    when w-column > 15 and < 23
                       set sort-by-pay-from to true
                    when w-column > 24 and < 32
                       set sort-by-pay-thru to true
                    when w-column > 43 and < 50
                       set sort-by-cash-dt to true
                 end-evaluate
              end-if
           end-if

           if eibaid = dfhpf7
              perform 7500-recover-temp-storage
                                       thru 7500-exit
              move ws-sorted-table     to ws-unsorted-table
              move spaces              to ws-sorted-table
              PERFORM 7400-DEL-TEMP-STOR-TABLE
                                       THRU 7400-EXIT
              move pi-max-sub          to ws-cntr
              perform 2000-desc-sort   thru 2000-exit
              move +1                  to pi-sub
              perform 0900-browse-fwd  thru 0900-exit
              GO TO 8100-SEND-INITIAL-MAP
           end-if

           if eibaid = dfhpf8
              perform 7500-recover-temp-storage
                                       thru 7500-exit
              move ws-sorted-table     to ws-unsorted-table
              move spaces to ws-sorted-table
              PERFORM 7400-DEL-TEMP-STOR-TABLE
                                       THRU 7400-EXIT
              move pi-max-sub to ws-cntr
              perform 3000-assc-sort   thru 3000-exit
              move +1                  to pi-sub
              perform 0900-browse-fwd  thru 0900-exit
              GO TO 8100-SEND-INITIAL-MAP
           end-if

           MOVE ER-0029                TO EMI-ERROR.                    
                                                                        
       0320-INPUT-ERROR.                                                
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF ENTERPFL = 0                                              
               MOVE -1                 TO ENTERPFL                      
           ELSE                                                         
               MOVE AL-UNBON           TO ENTERPFA                      
               MOVE -1                 TO ENTERPFL.                     

           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       0330-EDIT-DATA.                                                  
           IF NOT MODIFY-CAP                                            
               MOVE 'UPDATE'           TO  SM-READ                      
               PERFORM 9995-SECURITY-VIOLATION                          
               MOVE ER-0070            TO  EMI-ERROR                    
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
                                                                        
                                                                        
           move +1                     to m1
           MOVE 'F'                    TO DIRECTION-SWITCH             
           GO TO 0900-browse-fwd
                                                                        
           .
       0500-CREATE-TEMP-STORAGE.                                        
           MOVE EIBCPOSN               TO  PI-SAVE-CURSOR.              
           MOVE SPACES                 TO PI-FULL-SCREEN-IND.           
                                                                        
           EXEC CICS WRITEQ TS                                          
               QUEUE    (QID)                                           
               FROM     (PROGRAM-INTERFACE-BLOCK)                       
               LENGTH   (PI-COMM-LENGTH)                                
           END-EXEC.                                                    
      
                                                                        
           EJECT                                                        
       0600-RECOVER-TEMP-STORAGE.                                       
           MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.                 
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               QIDERR   (0690-QIDERR)                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS READQ TS                                           
               QUEUE    (QID)                                           
               INTO     (PROGRAM-INTERFACE-BLOCK)                       
               LENGTH   (PI-COMM-LENGTH)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS DELETEQ TS                                         
               QUEUE   (QID)                                            
           END-EXEC.                                                    
                                                                        
           MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.      
           MOVE 'F'                    TO  DIRECTION-SWITCH.            
                                                                        
           GO TO 0900-browse-fwd
      
           .
       0690-QIDERR.                                                     
           MOVE ER-0033                TO  EMI-ERROR.                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE LOW-VALUES             TO  EL150EO.                     
           MOVE -1                     TO  ENTERPFL.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
      
       0900-browse-fwd.

           if pi-sub > pi-max-sub
              move +1                  to pi-sub
           end-if
           move pi-sub                 to s1
           move +1                     to m1

           move pi-map-borr            to borro
           move pi-map-total-pd        to totalpdo
           move pi-map-total-int       to totinto
           move pi-carrier             to carro
           move pi-claim-no            to clmnoo
           move pi-cert-no             to certnoo
           move pi-cert-sfx            to crtsfxo

           perform varying s1 from s1 by +1 until
              (s1 > pi-max-sub)
              or (m1 > +12)
              perform 0950-bld-work-lines
                                       thru 0950-exit
              move work-line-1         to txtlineo (m1)
              add +1                   to m1
              move work-line-2         to txtlineo (m1)
              add +1                   to m1
           end-perform

           divide m1 by 2 giving pi-recs-last-page
              remainder ws-remainder

           move s1                     to pi-sub

           .
       0900-EXIT.
           EXIT.                                                        
      
       0910-browse-bwd.

              if pi-sub < +13
                 move +1               to pi-sub
              else
           if pi-sub > pi-max-sub
              compute pi-sub = pi-sub - +6 - pi-recs-last-page
           else
                 subtract +12 from pi-sub
              end-if
           end-if
           perform 0900-browse-fwd     thru 0900-exit
           go to 0910-exit

           .
       0910-exit.
           exit.

       0920-endbr.
      
           exec cics endbr
              dataset   ('ELTRLR')
              resp      (ws-response)
           end-exec
      
           .
       0920-exit.
           exit.
      
       0950-bld-work-lines.

           move spaces                 to work-line-1
                                          work-line-2

           move ws-srtd-pmt-type (s1)  to w-line-1-pmt-type
           move ws-srtd-check-amt (s1) to ws-pmt-amt-tmp
           move ws-pmt-amt-tmpx        to w-line-1-pd-amt
           move ws-line-no             to w-line-1-no

           move ws-srtd-check-no (s1)  to w-line-1-check-no

           if ws-srtd-pd-from-dt (s1) not = low-values and spaces
              move ws-srtd-pd-from-dt (s1)
                                       to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-1-edit
                                       to w-line-1-pd-from
              end-if
           end-if
           if ws-srtd-pd-thru-dt (s1) not = low-values and spaces
              move ws-srtd-pd-thru-dt (s1)
                                       to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-1-edit
                                       to w-line-1-pd-thru
              end-if
           end-if
082218     IF WS-SRTD-PD-DT (S1) = 'HP'
082218        MOVE 'ON HOLD'           TO W-LINE-1-PD-DT
082218     ELSE
           if ws-srtd-pd-dt (s1) not = low-values and spaces
              and high-values
              move ws-srtd-pd-dt (s1)  to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-1-edit
                                       to w-line-1-pd-dt
              end-if
           end-if
082218     END-IF
           if ws-srtd-void-dt (s1) not = low-values and spaces
              move ws-srtd-void-dt (s1) to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-1-edit
                                       to w-line-1-void-dt
              end-if
           end-if
           if ws-srtd-cashed-dt (s1) not = low-values and spaces
              move ws-srtd-cashed-dt (s1)
                                       to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-1-edit
                                       to w-line-1-cash-dt
              end-if
           end-if

           move s1                     to w-line-1-no
           move ws-srtd-payee (s1)     to w-line-2-payee

           .
       0950-exit.
           exit.

       1000-build-init-table.

           move spaces to ws-sorted-table ws-unsorted-table
           move +1                     to s1   *> subscript for table

           move low-values             to elmstr-key
           MOVE PI-COMPANY-CD          TO MSTR-COMP-CD
           MOVE PI-CARRIER             TO MSTR-CARRIER
           MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO
           MOVE PI-CERT-NO             TO MSTR-CERT-NO

           EXEC CICS READ
              DATASET   ('ELMSTR')
              RIDFLD    (ELMSTR-KEY)
              SET       (ADDRESS OF CLAIM-MASTER)
              resp      (ws-response)
           END-EXEC
      
           IF NOT RESP-NORMAL
              display ' something went wrong read ELMSTR '
                 pi-cert-no ' ' ws-response
              go to 0900-exit
           END-IF

           string
              cl-insured-last-name ' , '
              cl-insured-1st-name
              delimited by '  ' into pi-map-borr
           end-string

      **   MOVE CL-INSURED-LAST-NAME   TO LNAMEO
      **   MOVE CL-insured-1st-name    to fnameo
           move cl-total-paid-amt      to pi-map-total-pd
           if cl-total-int-paid not numeric
              move zeros               to pi-map-total-int
           else
              move cl-total-int-paid   to pi-map-total-int
           end-if

           MOVE LOW-VALUES             TO ELTRLR-KEY
           MOVE PI-COMPANY-CD          TO TRLR-COMP-CD
           MOVE PI-CARRIER             TO TRLR-CARRIER
           MOVE PI-CLAIM-NO            TO TRLR-CLAIM-NO
           MOVE PI-CERT-NO             TO TRLR-CERT-NO
           move +1000                  to trlr-seq-no
      
           exec cics startbr
              dataset   ('ELTRLR')
              ridfld    (eltrlr-key)
              gteq
              resp      (ws-response)
           end-exec
      
           if not resp-normal
              display ' something went wrong eltrlr startbr '
                 pi-carrier ' ' pi-claim-no ' ' pi-cert-no ' '
                 ws-response
              go to 1000-exit
           end-if

           .
       1000-get-next-trlr.
      
           EXEC CICS READNEXT
              DATASET   ('ELTRLR')
              RIDFLD    (ELTRLR-KEY)
              SET       (ADDRESS OF ACTIVITY-TRAILERS)
              resp      (ws-response)
           END-EXEC
      
           IF NOT RESP-NORMAL
              display ' something went wrong readnext eltrlr '
                 pi-cert-no ' ' ws-response
              perform 0920-endbr       thru 0920-exit
              perform 7200-disconnect  thru 7200-exit
              go to 1000-exit
           END-IF
      
           if pi-company-cd   = trlr-comp-cd
              and pi-carrier  = trlr-carrier
              and pi-claim-no = trlr-claim-no
              and pi-cert-no  = trlr-cert-no
              continue
           else
              perform 0920-endbr       thru 0920-exit
              perform 7200-disconnect  thru 7200-exit
              go to 1000-exit
           end-if
      
           if at-trailer-type = '2'    *>  payment trailer
              continue
           else
              go to 1000-get-next-trlr
           end-if

           perform 1010-build-table-rec thru 1010-exit
           add +1 to s1
           go to 1000-get-next-trlr

           .
       1000-exit.
           exit.

       1010-build-table-rec.

           evaluate at-payment-type
              when '1'
                 move 'P'              to ws-pmt-type (s1)
              when '2'
                 move 'F'              to ws-pmt-type (s1)
              when '3'
                 move 'L'              to ws-pmt-type (s1)
              when '4'
                 move 'A'              to ws-pmt-type (s1)
              when '5'
                 move 'C'              to ws-pmt-type (s1)
              when '6'
                 move 'N'              to ws-pmt-type (s1)
              when 'I'
                 move 'I'              TO ws-pmt-type (s1)
              when other
                 move 'O'              to ws-pmt-type (s1)
           end-evaluate

           move at-amount-paid         to ws-check-amt  (s1)
           if at-ach-payment = 'Y'
              move 'ACHPMNT'           to ws-check-no   (s1)
           else
              move at-check-no         to ws-check-no   (s1)
           end-if
           move at-paid-from-dt        to ws-pd-from-dt (s1)
           move at-paid-thru-dt        to ws-pd-thru-dt (s1)
082218
082218     IF AT-TO-BE-WRITTEN-DT  > LOW-VALUES
082218       AND AT-CHECK-WRITTEN-DT = LOW-VALUES
082218       AND AT-VOID-DT = LOW-VALUES
082218        MOVE 'HP'                TO WS-PD-DT      (S1)
082218        MOVE HIGH-VALUES         TO WS-KEY-DT     (S1)
082218     ELSE
           if at-check-written-dt = spaces or low-values
              move high-values         to ws-pd-dt      (s1)
                                          ws-key-dt     (s1)
           else
              move at-check-written-dt to ws-pd-dt      (s1)
                                          ws-key-dt (s1)  *> default
           end-if
082218     END-IF
           move at-void-dt             to ws-void-dt    (s1)
           move at-payees-name         to ws-payee      (s1)

           if not connected-to-db
              perform 7000-connect-to-db thru 7000-exit
           end-if

013017     if at-ach-payment = 'Y'
013017        go to 1010-ach-payment
013017     end-if
           
           if not connected-to-db   *>  Must have been an error
              continue
           else
              perform 7100-GET-CHK-CASHED-DT
                                       thru 7100-exit
           end-if

           go to 1010-build-date

           .
       1010-ach-payment.

           if not connected-to-db   *>  Must have been an error
              go to 1010-build-date
           end-if

           move at-carrier             to ws-carrier
           move cl-cert-state          to ws-state
           move cl-cert-account        to ws-account-no
           move at-cert-no             to ws-cert-no
           move at-claim-no            to ws-claim-no
           move zeros                  to ws-ach-check-no
           move at-check-no            to ws-ach-check-no (4:7)
           move spaces                 to ws-check-cashed-dt

           display ' about to select ' ws-ach-data

           if pi-company-id = 'CID'
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-ach-check-no
              end-exec
           else
              exec sql
                 SELECT
                    CASHED_DATE
                 INTO
                    :ws-check-cashed-dt
                 FROM
                    DCC_CLM_PMTS_ACH
                 WHERE
                        CARRIER   = :ws-carrier
                    and STATE     = :ws-state
                    and ACCOUNT   = :ws-account-no
                    and CERT_NO   = :ws-cert-no
                    and CLAIM_NO  = :ws-claim-no
                    and CHECK_NO  = :ws-ach-check-no
              end-exec
           end-if

           display ' check cashed dt ' ws-check-cashed-dt
           if ws-check-cashed-dt = spaces
              move low-values          to ws-cashed-dt (s1)
           else
              move spaces              to dc-greg-date-cymd-r
              string
                 ws-check-cashed-dt (1:4)
                 ws-check-cashed-dt (6:2)
                 ws-check-cashed-dt (9:2)
                 delimited by size into dc-greg-date-cymd
              end-string
              move 'L'                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
                 and (dc-bin-date-1 > at-check-written-dt)
                 move dc-bin-date-1    to ws-cashed-dt (s1)
              else
                 move low-values       to ws-cashed-dt (s1)
                 display ' error cnvt cash dt ' dc-error-code ' '
                    ws-check-cashed-dt
              end-if
           end-if


           .
       1010-build-date.

           if ws-check-cashed-dt = spaces
              move low-values          to ws-cashed-dt (s1)
           else
111714        move spaces              to dc-greg-date-cymd-r
111714        string
111714           ws-check-cashed-dt (1:4)
111714           ws-check-cashed-dt (6:2)
111714           ws-check-cashed-dt (9:2)
111714           delimited by size into dc-greg-date-cymd
111714        end-string
111714        move 'L'                 to dc-option-code
              perform 9700-link-date-convert
                                       thru 9700-exit
              if no-conversion-error
111816           and (dc-bin-date-1 > at-check-written-dt)
                 move dc-bin-date-1    to ws-cashed-dt (s1)
              else
111816           move low-values       to ws-cashed-dt (s1)
                 display ' error cnvt cash dt ' dc-error-code ' '
                    ws-check-cashed-dt
              end-if
           end-if

           .
       1010-exit.
           exit.

       2000-desc-sort.

           perform varying s1 from +1 by +1 until s1 > pi-max-sub
              evaluate true
                 when sort-by-pay-from
                    move ws-pd-from-dt (s1)
                                       to ws-key (s1)
                 when sort-by-pay-thru
                    move ws-pd-thru-dt (s1)
                                       to ws-key (s1)
                 when sort-by-cash-dt
                    move ws-cashed-dt (s1)
                                       to ws-key (s1)
                 when other
                    if ws-pd-dt (s1) = low-values
                       move high-values to ws-pd-dt (s1)
                                           ws-key (s1)
                    end-if
              end-evaluate
           end-perform
           move pi-max-sub             to ws-max-sub
                                          ws-cntr
           move +0                     to s3
           move +1                     to s2
           perform until (ws-cntr = zero) or (s3 > 700)
              move +0 to ws-hold-s1
              move low-values          to ws-hold-key
              perform varying s1 from +1 by +1 until
                 (s1 > ws-max-sub)
                 if (ws-sorted-sw (s1) not = 'Y')
                    and (ws-key (s1) >= ws-hold-key)
                    move ws-key (s1)   to ws-hold-key
                    move s1            to ws-hold-s1
                 end-if
              end-perform
              if ws-hold-s1 not = zeros
                 move ws-hold-s1         to s1
                 move ws-key        (s1) to ws-srtd-key        (s2)
                 move ws-check-no   (s1) to ws-srtd-check-no   (s2)
                 move ws-pd-from-dt (s1) to ws-srtd-pd-from-dt (s2)
                 move ws-pd-thru-dt (s1) to ws-srtd-pd-thru-dt (s2)
                 move ws-pd-dt      (s1) to ws-srtd-pd-dt      (s2)
                 move ws-cashed-dt  (s1) to ws-srtd-cashed-dt  (s2)
                 move ws-void-dt    (s1) to ws-srtd-void-dt    (s2)
                 move ws-check-amt  (s1) to ws-srtd-check-amt  (s2)
                 move ws-pmt-type   (s1) to ws-srtd-pmt-type   (s2)
                 move ws-payee      (s1) to ws-srtd-payee      (s2)
                 move 'Y'                to ws-sorted-sw       (s1)
                 subtract 1 from ws-cntr
                 add +1 to s2
              end-if
              add 1 to s3
           end-perform

           perform 7300-CREATE-TEMP-STORAGE
                                       thru 7300-exit

           .
       2000-exit.
           exit.

       3000-assc-sort.

           perform varying s1 from +1 by +1 until s1 > pi-max-sub
              evaluate true
                 when sort-by-pay-from
                    move ws-pd-from-dt (s1)
                                       to ws-key (s1)
                 when sort-by-pay-thru
                    move ws-pd-thru-dt (s1)
                                       to ws-key (s1)
                 when sort-by-cash-dt
                    move ws-cashed-dt (s1)
                                       to ws-key (s1)
                 when other
                    if ws-pd-dt (s1) = high-values
                       move low-values to ws-pd-dt (s1)
                                          ws-key (s1)
                    end-if
           end-perform
           move high-values            to ws-key (pi-max-sub + 1)
           move pi-max-sub to ws-max-sub ws-cntr
           move +0                     to s3
           move +1                     to s2
           perform until (ws-cntr = zero) or (s3 > 700)
              move +0 to ws-hold-s1
              move high-values         to ws-hold-key
              perform varying s1 from +1 by +1 until
                 (s1 > ws-max-sub)
                 if (ws-sorted-sw (s1) not = 'Y')
                    and (ws-key (s1) <= ws-hold-key)
                    move ws-key (s1)   to ws-hold-key
                    move s1            to ws-hold-s1
                 end-if
              end-perform
              if ws-hold-s1 not = zeros
                 move ws-hold-s1         to s1
                 move ws-key        (s1) to ws-srtd-key        (s2)
                 move ws-check-no   (s1) to ws-srtd-check-no   (s2)
                 move ws-pd-from-dt (s1) to ws-srtd-pd-from-dt (s2)
                 move ws-pd-thru-dt (s1) to ws-srtd-pd-thru-dt (s2)
                 move ws-pd-dt      (s1) to ws-srtd-pd-dt      (s2)
                 move ws-cashed-dt  (s1) to ws-srtd-cashed-dt  (s2)
                 move ws-void-dt    (s1) to ws-srtd-void-dt    (s2)
                 move ws-check-amt  (s1) to ws-srtd-check-amt  (s2)
                 move ws-pmt-type   (s1) to ws-srtd-pmt-type   (s2)
                 move ws-payee      (s1) to ws-srtd-payee      (s2)
                 move 'Y'                to ws-sorted-sw       (s1)
                 subtract 1 from ws-cntr
                 add +1 to s2
              end-if
              add 1 to s3
           end-perform

           perform 7300-CREATE-TEMP-STORAGE
                                       thru 7300-exit

           .
       3000-exit.
           exit.

       7000-CONNECT-TO-DB.
      
           MOVE 'NTCSO2_LOGIC'         TO SVR
           MOVE 'sa'                   TO USR
           MOVE 'ntcso2'               TO PASS
      
111714*    MOVE 'NTSQLTST2_LOGIC'      TO SVR
111714*    MOVE 'sa'                   TO USR
111714*    MOVE 'sql2008r2'            TO PASS

      *    move 'NTSQLTST2UAT_Logic'   to svr
      *    move 'sa'                   to usr
      *    move 'ntcso2'               to pass
      *    move 'sql2008r2'            to pass

           STRING
               USR DELIMITED SPACE
               "." DELIMITED SIZE
               PASS DELIMITED SPACE INTO USR-PASS
           END-STRING
       
           EXEC SQL
              CONNECT TO :SVR USER :USR-PASS
           END-EXEC
       
           IF SQLCODE NOT = 0
              DISPLAY "ERROR: CANNOT CONNECT "
              DISPLAY SQLCODE
              DISPLAY SQLERRMC
              GO TO 7000-EXIT
           else
              set connected-to-db to true
           END-IF

           .
       7000-EXIT.
           EXIT.

       7100-GET-CHK-CASHED-DT.
      
051215     move spaces                 to ws-check-cashed-dt
111714     move '2'                    to ws-draft-or-check  *>  draft
111714     if at-check-written-dt not = low-values and spaces
111714        evaluate true
060315           when (at-check-written-dt > X'ACFE')    *> 04/30/2015
090415           and (pi-company-id not = 'DCC')
111714              move '1'           to ws-draft-or-check  *>  check
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
062717           when (at-check-written-dt > X'B016')    *> 05/22/2017
013017           and (pi-company-id = 'DCC')
013017              move '1'           to ws-draft-or-check  *>  check
013017              move '000'         to ws-check-number (1:3)
013017              move at-check-no   to ws-check-number (4:7)
111714           when at-payment-type = 'I'
111714              move '000'         to ws-check-number (1:3)
111714              move at-check-no   to ws-check-number (4:7)
111714           when other
111714              move '0'           to ws-check-number (1:1)
111714              move at-check-no (1:1)
111714                                 to ws-check-number (2:1)
111714              move '00'          to ws-check-number (3:2)
111714              move at-check-no (2:6)
111714                                 to ws-check-number (5:6)
111714        end-evaluate
051215     else   *>  no check written date yet
051215        go to 7100-exit
111714     end-if

111714     evaluate true
111714        when pi-company-id = 'AHL'
111714           move '%AHL%'          to ws-check-company
111714        when pi-company-id = 'DCC'
111714           move '%DCC%'          to ws-check-company
020816        when pi-company-id = 'VPP'
020816           move '%VPP%'          to ws-check-company
062121        when pi-company-id = 'FNL'
062121           move '%FNL%'          to ws-check-company
111714        when other   *>   CID s/b   CSO
111714           move '%CSO%'          to ws-check-company
111714     end-evaluate

           move at-payment-type        to ws-check-type
111714     move at-claim-no            to ws-claim-number
           move at-amount-paid         to ws-check-amt-tmp
           move ws-check-amt-tmpx      to ws-check-amount

           EXEC SQL
             CALL LogicPaidBankChkCashedDt
111714                 @draftorcheck = :WS-DRAFT-OR-CHECK,
                       @checktype = :WS-CHECK-TYPE,
111714                 @claimnumber = :WS-CLAIM-NUMBER,
111714                 @checknumber = :WS-CHECK-NUMBER,
                       @checkamount = :WS-CHECK-AMOUNT,
                       @checkcompany = :WS-CHECK-COMPANY,
                       @checkcasheddate = :WS-CHECK-CASHED-DT OUT
           END-EXEC
           IF SQLCODE NOT = 0
              MOVE SPACES TO WS-CHECK-CASHED-DT
               DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
               DISPLAY ' SQL RETURN CODE ' SQLCODE
               DISPLAY ' SQL ERR MESS    ' SQLERRMC
              GO TO 7100-EXIT
           END-IF

           .
       7100-EXIT.
           EXIT.
      
       7110-CHECK-MANUAL.
      
           MOVE AT-PAYMENT-TYPE     TO WS-CHECK-TYPE
           MOVE AT-CHECK-NO         TO WS-CHECK-NUMBER
           MOVE AT-AMOUNT-PAID      TO WS-CHECK-AMT-TMP
           MOVE WS-CHECK-AMT-TMPX   TO WS-CHECK-AMOUNT
           MOVE PI-COMPANY-ID       TO WS-CHECK-COMPANY
           MOVE SPACES              TO WS-CHECK-CASHED-DT
      
           EXEC SQL
             CALL LogicPaidBankChkCashedDtManual
                       @checktype = :WS-CHECK-TYPE,
                       @checknumber = :WS-CHECK-NUMBER,
                       @checkamount = :WS-CHECK-AMOUNT,
                       @checkcompany = :WS-CHECK-COMPANY,
                       @checkcasheddate = :WS-CHECK-CASHED-DT OUT
           END-EXEC
      
           IF SQLCODE NOT = 0
              MOVE SPACES TO WS-CHECK-CASHED-DT
      *        DISPLAY "ERROR: DID NOT RETURN CHK CASHED DT "
      *        DISPLAY ' SQL RETURN CODE ' SQLCODE
      *        DISPLAY ' SQL ERR MESS    ' SQLERRMC
      *       GO TO 7110-EXIT
           END-IF
      
           .
       7110-EXIT.
           EXIT.
      
       7200-DISCONNECT.
      
           EXEC SQL
              DISCONNECT
           END-EXEC
           .
       7200-EXIT.
           EXIT.

       7300-CREATE-TEMP-STORAGE.

           EXEC CICS WRITEQ TS
               QUEUE    (WS-TABLE-QID)
               FROM     (WS-SORTED-TABLE)
               LENGTH   (WS-TABLE-LENGTH)
               ITEM     (WS-ITEM-COUNT)
               resp     (ws-response)
           END-EXEC
           if not resp-normal
              display ' 7300 create error ' ws-response
                 ' ' ws-table-qid ' ' ws-item-count
           end-if

           .
       7300-EXIT.                                                       
           EXIT.                                                        


       7400-DEL-TEMP-STOR-TABLE.

           EXEC CICS DELETEQ TS
               QUEUE    (WS-TABLE-QID)
               resp     (ws-response)
           END-EXEC
           if not resp-normal
              display ' 7400 delete error ' ws-response
                 ' ' ws-table-qid ' ' ws-item-count
           end-if

           .
       7400-EXIT.                                                       
           EXIT.                                                        

       7500-RECOVER-TEMP-STORAGE.

           EXEC CICS READQ TS
               QUEUE    (WS-TABLE-QID)
               INTO     (WS-SORTED-TABLE)
               LENGTH   (WS-TABLE-LENGTH)
               ITEM     (WS-ITEM-COUNT)
               resp     (ws-response)
           END-EXEC

           if not resp-normal
              display ' 7500 recover error ' ws-response ' '
                 ws-table-qid ' ' ws-table-length ' ' ws-item-count
           end-if

           .
       7500-EXIT.                                                       
           EXIT.                                                        
                                                                        
       8000-LOAD-ERROR-MESSAGES.                                        
           IF EMI-NO-ERRORS                                             
               GO TO 8000-EXIT.                                         
                                                                        
           IF EMI-NUMBER-OF-LINES = 1                                   
               MOVE EMI-LINE1          TO  ERRMSG1O                     
               GO TO 8000-EXIT.                                         
                                                                        
           MOVE EMI-LINE1              TO  ERRMSG1O.                    
                                                                        
       8000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       8100-SEND-INITIAL-MAP.                                           
      
           move -1                     to enterpfl
           MOVE SAVE-DATE              TO RUNDTEO.                     
           MOVE EIBTIME                TO TIME-IN.                     
           MOVE TIME-OUT               TO RUNTIMEO.                    
           PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
      
      
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL150EO)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
       8200-SEND-DATAONLY.                                              
      
           MOVE SAVE-DATE              TO  RUNDTEO.                     
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  RUNTIMEO.                    
           PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             
      
           EXEC CICS SEND                                               
               MAP      (MAP-NAME)                                      
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL150EO)                                       
               DATAONLY                                                 
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
                                                                        
       8400-NOT-FOUND.                                                  
           IF FILE-SWITCH = 'BENE'                                      
               MOVE ER-0282            TO  EMI-ERROR.                   
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF PASS-SWITCH = 'A'                                         
               GO TO 8100-SEND-INITIAL-MAP                              
           ELSE                                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
       8500-FILE-NOTOPEN.                                               
                                                                        
                                                                        
           IF FILE-SWITCH = 'CERT'                                      
               MOVE ER-0169            TO  EMI-ERROR.                   
                                                                        
                                                                        
           IF FILE-SWITCH = 'MSTR'                                      
               MOVE ER-0154            TO  EMI-ERROR.                   
                                                                        
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF PASS-SWITCH = 'A'                                         
               GO TO 8100-SEND-INITIAL-MAP                              
           ELSE                                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
           MOVE XCTL-005               TO  PGM-NAME.                    
           GO TO 9300-XCTL.                                             
                                                                        
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
           MOVE '150E'                 TO  PI-CURRENT-SCREEN-NO.        
                                                                        
           EXEC CICS RETURN                                             
               TRANSID    (TRANS-ID)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9200-RETURN-MAIN-MENU.                                           
           MOVE XCTL-126               TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       9300-XCTL.                                                       
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR   (9350-NOT-FOUND)                              
           END-EXEC.                                                    
                                                                        
           EXEC CICS XCTL                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9350-NOT-FOUND.                                                  
           MOVE ER-0923                TO EMI-ERROR.                    
           PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.               
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-010               TO  PGM-NAME.                    
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR   (8300-SEND-TEXT)                              
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          
           MOVE ' '                    TO  PI-ENTRY-CD-1.               
           MOVE XCTL-005               TO  PGM-NAME.                    
           MOVE PGM-NAME               TO  LOGOFF-PGM.                  
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
           GO TO 9300-XCTL.                                             
                                                                        
       9700-LINK-DATE-CONVERT.                                          
           MOVE LINK-ELDATCV           TO PGM-NAME.                     
                                                                        
           EXEC CICS LINK                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (DATE-CONVERSION-DATA)                        
               LENGTH     (DC-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9700-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9800-DEEDIT.                                                     
                                                                        
           EXEC CICS BIF DEEDIT                                         
               FIELD   (WS-DEEDIT-FIELD)                                
               LENGTH  (WS-DEEDIT-LENGTH)                               
           END-EXEC.                                                    
                                                                        
       9800-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9900-ERROR-FORMAT.                                               
           IF NOT EMI-ERRORS-COMPLETE                                   
               MOVE LINK-001           TO PGM-NAME                      
               EXEC CICS LINK                                           
                   PROGRAM    (PGM-NAME)                                
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
                   LENGTH     (EMI-COMM-LENGTH)                         
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE -1                     TO  ENTERPFL.                    
           MOVE LINK-004               TO  PGM-NAME.                    
                                                                        
           MOVE DFHEIBLK               TO  EMI-LINE1                    
           EXEC CICS LINK                                               
               PROGRAM   (PGM-NAME)                                     
               COMMAREA  (EMI-LINE1)                                    
               LENGTH    (72)                                           
           END-EXEC.                                                    
                                                                        
           MOVE EMI-LINE1              TO  ERRMSG1O.                    
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       EJECT                                                            
       9995-SECURITY-VIOLATION.                                         
                                   COPY ELCSCTP.                        
                                                                        EL1502
