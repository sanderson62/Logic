00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL1503.                              
00008 *                                                                 
00009 *AUTHOR.     PABLO
00010 *            OMAHA, NE.
00011                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    TRANSACTION - E023 - CERT CLAIM HISTORY
062602******************************************************************
062602*                   C H A N G E   L O G
062602*
062602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062602*-----------------------------------------------------------------
062602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062602* EFFECTIVE    NUMBER
062602*-----------------------------------------------------------------
093013* 093013    2013021100002  PEMA  NEW PROGRAM
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
021615* 021615  CR2014062700002  PEMA  ADD XCTL TO EL1504
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025                                                                   
00026  ENVIRONMENT DIVISION.                                            
00027                                                                   
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         
00031  77  FILLER  PIC X(32)  VALUE '********************************'. 
00032  77  FILLER  PIC X(32)  VALUE '*   EL1503 WORKING STORAGE     *'. 
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
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
       77  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
           88  PDEF-FOUND                   VALUE 'Y'.
       77  wk1                         pic 999 value zeros.
       77  wk2                         pic 999 value zeros.
       77  ws-eracct-sw                pic x  value spaces.
           88  acct-found               value 'Y'.
100314 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.

00035                                      COPY ELCSCTM.                
00036                                                                   
00037                                      COPY ELCSCRTY.               
00038                                                                   
00040  01  WS-DATE-AREA.                                                
00041      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.    
00042      12  SAVE-DATE-CCYYMMDD.                                      
00043          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.    
00044          16  SAVE-DATE-YMD.                                       
00045              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.    
00046              20  FILLER              PIC X(4)    VALUE SPACES.    
00047      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    
00048                                                                   
       01  output-screen-work-area.
           05  os-prev-key.
               10  os-prev-clm-type        pic x.
               10  os-prev-ins-type        pic x.
           05  ws-pd-bens                  pic 999 value zeros.
           05  ws-cov-rem-bens             pic s999 value zeros.

00049  01  STANDARD-AREAS.                                              
00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     
00051      12  MAP-NAME                    PIC X(8)    VALUE 'EL150D'.  
00052      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1503S'. 
00053      12  TRANS-ID                    PIC X(4)    VALUE 'E023'.    
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL1503'.  
00055      12  PGM-NAME                    PIC X(8).                    
00056      12  TIME-IN                     PIC S9(7).                   
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00058          16  FILLER                  PIC X.                       
00059          16  TIME-OUT                PIC 99V99.                   
00060          16  FILLER                  PIC XX.                      
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   
00063      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.   
           12  xctl-150                    pic x(8)    value 'EL150'.
           12  xctl-1504                   pic x(8)    value 'EL1504'.
100518     12  xctl-1285                   pic x(8)    value 'EL1285'.
00064      12  XCTL-142                    PIC X(8)    VALUE 'EL142'.   
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. 
00068      12  FILE-ID                     PIC X(8).                    
00069      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00070      12  WS-ITEM-COUNT               PIC S9(4)   VALUE +1    COMP.

00072  01  MISC-WORK-AREAS.                                             

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

00074      12  W-CALLING-PGM               PIC X(8).                    
00075      12  QID.                                                     
00076          16  QID-TERM                PIC X(4).                    
00077          16  FILLER                  PIC X(4)    VALUE '150D'.
00078      12  WS-TABLE-QID.                                            
00079          16  WS-QID-TERM             PIC X(4).                    
00080          16  FILLER                  PIC X(4)    VALUE 'TBLE'.    
00081                                                                   
00082      12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.
00083      12  WS-TABLE-LENGTH             PIC S9(4)   VALUE +3200 COMP.
00084      12  PASS-SWITCH                 PIC X       VALUE 'A'.       
00085      12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.
00086      12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.    
00087      12  WS-SUB                      PIC 9       VALUE 0.         
00088      12  SUB                         PIC 9       VALUE 1.         
00089      12  SUB-1                       PIC 9       VALUE 1.         
00090      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.    
00091      12  DIRECTION-SWITCH            PIC X       VALUE 'N'.       
00092      12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.       
00093          88  RECORDS-READ                        VALUE 'Y'.       
00094      12  WS-ENDBR-SW                 PIC X       VALUE 'N'.       
00095      12  SAVE-CONTROL                PIC X(39).                   
00111      12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.
00112      12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.     
00113      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD             
00114                                      PIC S9(16).                  
00115                                                                   
00132      12  WS-UPDATE-SW                PIC X       VALUE 'N'.       
00134                                                                   
00135      12  WS-WORK-DATE.                                            
00136          16  WS-WORK-MM              PIC 99      VALUE ZEROS.     
00137          16  WS-WORK-DD              PIC 99      VALUE ZEROS.     
00138          16  WS-WORK-YY              PIC 99      VALUE ZEROS.     
00139                                                                   
00152                                                                   
00154      12  HOLD-SUB                    PIC 9(4)    VALUE ZEROS.     
00155      12  SUB-2                       PIC 9(4)    VALUE ZEROS.     
00156      12  SUB-3                       PIC 9(4)    VALUE ZEROS.     
00157      12  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.     

00159  01  WS-UNSORTED-TABLE.                                           
00160      12  WS-UNSRTD-TABLE   OCCURS 25 TIMES.                      
               16  ws-key.
                   20  ws-clm-type     pic x.
                   20  ws-ins-type     pic x.
                   20  ws-ben-per      pic 99.
                   20  ws-inc-dt       pic xx.
               16  ws-excl-per         pic 999.
               16  ws-cov-ends         pic 999.
               16  ws-acc-per          pic 999.
               16  ws-max-bens         pic 999.
               16  ws-rec-mos          pic 99.
               16  ws-max-exten        pic 99.
               16  ws-status           pic x.
               16  ws-pd-thru-dt       pic xx.
               16  ws-claim-no         pic x(7).
               16  ws-max-moben        pic s9(7)v99 comp-3.
               16  ws-total-paid       pic s9(7)v99 comp-3.
               16  ws-rem-bens         pic 999.
               16  ws-sorted-sw        pic x.

00166  01  WS-SORTED-TABLE.                                             
00167      12  WS-SRTD-TABLE OCCURS 25 TIMES.
               16  ws-srtd-key.
                   20  ws-srtd-clm-type pic x.
                   20  ws-srtd-ins-type pic x.
                   20  ws-srtd-ben-per pic 99.
                   20  ws-srtd-inc-dt  pic xx.
               16  ws-srtd-excl-per    pic 999.
               16  ws-srtd-cov-ends    pic 999.
               16  ws-srtd-acc-per     pic 999.
               16  ws-srtd-max-bens    pic 999.
               16  ws-srtd-rec-mos     pic 99.
               16  ws-srtd-max-exten   pic 99.
               16  ws-srtd-status      pic x.
               16  ws-srtd-pd-thru-dt  pic xx.
               16  ws-srtd-claim-no    pic x(7).
               16  ws-srtd-max-moben   pic s9(7)v99 comp-3.
               16  ws-srtd-total-paid  pic s9(7)v99 comp-3.
               16  ws-srtd-rem-bens    pic 999.
               16  ws-srtd-sw          pic x.

00176  01  ACCESS-KEYS.                                                 
00626      12  ERACCT-KEY.                                      
00627          16  ERACCT-PARTIAL-KEY.                          
00628              20  ACCT-COMP-CD    PIC X.                   
00629              20  ACCT-CARRIER    PIC X.                   
00630              20  ACCT-GROUPING   PIC X(6).                
00631              20  ACCT-STATE      PIC XX.                  
00632              20  ACCT-ACCOUNT    PIC X(10).               
00633          16  ACCT-EXP-DT         PIC XX.                  
00634          16  acct-filler         PIC X(4) VALUE SPACES.   
00177      12  ELMSTR-KEY.                                              
00178          16  MSTR-COMP-CD            PIC X.                       
00179          16  MSTR-CARRIER            PIC X.                       
00180          16  MSTR-CLAIM-NO           PIC X(7).                    
00181          16  MSTR-CERT-NO.                                        
00182              20  MSTR-CERT-NO-PRIME  PIC X(10).                   
00183              20  MSTR-CERT-NO-SUFX   PIC X.                       
00189      12  ELCERT-KEY.                                              
00190          16  CERT-COMP-CD            PIC X.                       
00191          16  CERT-CARRIER            PIC X.                       
00192          16  CERT-GROUPING           PIC X(6).                    
00193          16  CERT-STATE              PIC XX.                      
00194          16  CERT-ACCOUNT            PIC X(10).                   
00195          16  CERT-EFF-DT             PIC XX.                      
00196          16  CERT-CERT-NO.                                        
00197              20  CERT-CERT-NO-PRIME  PIC X(10).                   
00198              20  CERT-CERT-NO-SUFX   PIC X.                       
00189      12  ELCRTT-KEY.                                              
00190          16  CRTT-COMP-CD            PIC X.                       
00191          16  CRTT-CARRIER            PIC X.                       
00192          16  CRTT-GROUPING           PIC X(6).                    
00193          16  CRTT-STATE              PIC XX.                      
00194          16  CRTT-ACCOUNT            PIC X(10).                   
00195          16  CRTT-EFF-DT             PIC XX.                      
00196          16  CRTT-CERT-NO            PIC X(11).                   
00197          16  CRTT-REC-TYPE           PIC X.
00240                                                                   
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.

00242  01  ERROR-MESSAGES.                                              
00243      12  ER-0000                     PIC X(4)    VALUE '0000'.    
00244      12  ER-0004                     PIC X(4)    VALUE '0004'.    
00245      12  ER-0008                     PIC X(4)    VALUE '0008'.    
00246      12  ER-0029                     PIC X(4)    VALUE '0029'.    
00247      12  ER-0033                     PIC X(4)    VALUE '0033'.    
00248      12  ER-0042                     PIC X(4)    VALUE '0042'.    
00249      12  ER-0068                     PIC X(4)    VALUE '0068'.    
00250      12  ER-0070                     PIC X(4)    VALUE '0070'.    
00251      12  ER-0130                     PIC X(4)    VALUE '0130'.    
00252      12  ER-0154                     PIC X(4)    VALUE '0154'.    
00253      12  ER-0169                     PIC X(4)    VALUE '0169'.    
00254      12  ER-0172                     PIC X(4)    VALUE '0172'.    
00255      12  ER-0190                     PIC X(4)    VALUE '0190'.    
00256      12  ER-0204                     PIC X(4)    VALUE '0204'.    
00257      12  ER-0206                     PIC X(4)    VALUE '0206'.    
00258      12  ER-0282                     PIC X(4)    VALUE '0282'.    
00259      12  ER-0303                     PIC X(4)    VALUE '0303'.    
00260      12  ER-0334                     PIC X(4)    VALUE '0334'.    
00261      12  ER-0335                     PIC X(4)    VALUE '0335'.    
00262      12  ER-0336                     PIC X(4)    VALUE '0336'.    
00263      12  ER-0337                     PIC X(4)    VALUE '0337'.    
00264      12  ER-0338                     PIC X(4)    VALUE '0338'.    
00265      12  ER-0376                     PIC X(4)    VALUE '0376'.    
00266      12  ER-0412                     PIC X(4)    VALUE '0412'.    
00267      12  ER-0413                     PIC X(4)    VALUE '0413'.    
00268      12  ER-0660                     PIC X(4)    VALUE '0660'.    
00269      12  ER-0661                     PIC X(4)    VALUE '0661'.    
00270      12  ER-0662                     PIC X(4)    VALUE '0662'.    
00271      12  ER-0663                     PIC X(4)    VALUE '0663'.    
00272      12  ER-0664                     PIC X(4)    VALUE '0664'.    
00273      12  ER-0665                     PIC X(4)    VALUE '0665'.    
00274      12  ER-0666                     PIC X(4)    VALUE '0666'.    
00275      12  ER-0667                     PIC X(4)    VALUE '0667'.    
00276      12  ER-0672                     PIC X(4)    VALUE '0672'.    
00277      12  ER-0776                     PIC X(4)    VALUE '0776'.    
00278      12  ER-0800                     PIC X(4)    VALUE '0800'.    
00279      12  ER-0801                     PIC X(4)    VALUE '0801'.    
00280      12  ER-0816                     PIC X(4)    VALUE '0816'.    
00281      12  ER-0823                     PIC X(4)    VALUE '0823'.    
00282      12  ER-0833                     PIC X(4)    VALUE '0833'.    
00283      12  ER-0835                     PIC X(4)    VALUE '0835'.    
00284      12  ER-0849                     PIC X(4)    VALUE '0849'.    
00285      12  ER-0919                     PIC X(4)    VALUE '0919'.    
00286      12  ER-0920                     PIC X(4)    VALUE '0920'.    
00287      12  ER-0921                     PIC X(4)    VALUE '0921'.    
00288      12  ER-0922                     PIC X(4)    VALUE '0922'.    
00289      12  ER-0923                     PIC X(4)    VALUE '0923'.    
00290      12  ER-0925                     PIC X(4)    VALUE '0925'.    
00291      12  ER-0939                     PIC X(4)    VALUE '0939'.    
00292      12  ER-0940                     PIC X(4)    VALUE '0940'.    
00293      12  ER-0941                     PIC X(4)    VALUE '0941'.    
00294      12  ER-0942                     PIC X(4)    VALUE '0942'.    
00295      12  ER-0946                     PIC X(4)    VALUE '0946'.    
00296      12  ER-0947                     PIC X(4)    VALUE '0947'.    
00297      12  ER-0948                     PIC X(4)    VALUE '0948'.    
00298      12  ER-0949                     PIC X(4)    VALUE '0949'.    
00299      12  ER-0950                     PIC X(4)    VALUE '0950'.    
00300      12  ER-0951                     PIC X(4)    VALUE '0951'.    
00301      12  ER-0952                     PIC X(4)    VALUE '0952'.    
00302      12  ER-0954                     PIC X(4)    VALUE '0954'.    
00303      12  ER-0974                     PIC X(4)    VALUE '0974'.    
00304      12  ER-0975                     PIC X(4)    VALUE '0975'.    
00305      12  ER-2378                     PIC X(4)    VALUE '2378'.    
00306      12  ER-2379                     PIC X(4)    VALUE '2379'.    
00307      12  ER-7999                     PIC X(4)    VALUE '7999'.    
062602     12  ER-8003                     PIC X(4)    VALUE '8003'.    
00308      12  ER-8051                     PIC X(4)    VALUE '8051'.    
00309      12  ER-8052                     PIC X(4)    VALUE '8052'.    
00310      12  ER-8053                     PIC X(4)    VALUE '8053'.    
00311      12  ER-8054                     PIC X(4)    VALUE '8054'.    
00312      12  ER-8055                     PIC X(4)    VALUE '8055'.    
00313      12  ER-8056                     PIC X(4)    VALUE '8056'.    
00314      12  ER-8057                     PIC X(4)    VALUE '8057'.    
00315      12  ER-8058                     PIC X(4)    VALUE '8058'.    
00316      12  ER-8059                     PIC X(4)    VALUE '8059'.    
00317      12  ER-8060                     PIC X(4)    VALUE '8060'.    
00318      12  ER-8061                     PIC X(4)    VALUE '8061'.    
00319      12  ER-8062                     PIC X(4)    VALUE '8062'.    
00320      12  ER-8063                     PIC X(4)    VALUE '8063'.    
00321      12  ER-8064                     PIC X(4)    VALUE '8064'.    
00322      12  ER-8065                     PIC X(4)    VALUE '8065'.    
00323      12  ER-8066                     PIC X(4)    VALUE '8066'.    
00324      12  ER-8152                     PIC X(4)    VALUE '8152'.    
00325      12  ER-8153                     PIC X(4)    VALUE '8153'.    
00326      12  ER-8154                     PIC X(4)    VALUE '8154'.    
00327      12  ER-8155                     PIC X(4)    VALUE '8155'.    
00328      12  ER-9211                     PIC X(4)    VALUE '9211'.    
00329      12  ER-9883                     PIC X(4)    VALUE '9883'.    
00330                                                                   

00332  01  TEXT-WORK-AREAS.
           05  ws-cov-type             pic x(4) value spaces.

           05  M1                         PIC S999 COMP-3 VALUE +0.
           05  WS-MAP-OUTPUT OCCURS 13.
               10  WSM-LINE-NO            PIC 99.
               10  FIL                    PIC X.
               10  WSM-INS-TYPE           PIC XXXX.
               10  FIL                    PIC XX.
               10  WSM-BEN-PER            PIC 99.
               10  FIL                    PIC XXX.
               10  WSM-STATUS             PIC X(6).
               10  FIL                    PIC XX.
               10  WSM-CLAIM-NO           PIC X(7).
               10  FIL                    PIC XX.
               10  WSM-INC-DATE           PIC X(10).
               10  FIL                    PIC XX.
               10  WSM-PD-THRU-DT         PIC X(10).
               10  FIL                    PIC XX.
               10  WSM-TOTAL-PAID         PIC ZZZ,ZZ9.99.
               10  FIL                    PIC XX.
               10  WSM-MAX-BENS           PIC Z99.
               10  FIL                    PIC X.
               10  WSM-PD-BENS            PIC Z99.
               10  WSM-PART               PIC X.
               10  WSM-REM-BENS           PIC Z99.

00574                                  COPY ELCDATE.                
00576                                  COPY ELCLOGOF.               
00578                                  COPY ELCATTR.                
00580                                  COPY ELCEMIB.                
00590                                  COPY ELCINTF.                

00592      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.              

062602         16  FILLER                  PIC x(2).                    
062602         16  pi-el142-priority       pic x.
062602         16  filler                  pic x.
00594          16  PI-MAP-NAME             PIC X(8).                    
00605          16  FILLER                  PIC X(8).                    
00625          16  PI-RECORD-COUNT              PIC S9        COMP-3.   
00626          16  PI-END-OF-FILE               PIC S9        COMP-3.   
00627          16  PI-SAVE-CURSOR               PIC S9(4)     COMP.     
00628          16  PI-PURGED-SW                 PIC X.                  
00629              88  CLAIM-IS-PURGED                 VALUE 'Y'.       
00630          16  PI-LINE-NO                   PIC 9.                  
00631          16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.       
00632          16  PI-FIRST-TIME-SW             PIC X.                  
00633              88  FIRST-TIME                      VALUE 'Y'.       
00642                                                                   
               
00643          16  PI-PREV-CLAIM-NO             PIC X(7).               
00644          16  PI-PREV-CARRIER              PIC X.                  
00645          16  PI-PRIME-CERT-NO.                                    
00646              20  PI-PRIME-CERT            PIC X(10).              
00647              20  PI-PRIME-SUFX            PIC X.                  
00648          16  PI-PRIME-HDG                 PIC X(12).              
00649          16  PI-MAX-SUB                   PIC 9(4).               
00650          16  PI-PAY-TYPE                  PIC X.                  
00651          16  PI-FULL-SCREEN-IND           PIC X.                  
00652              88  PI-FULL-SCREEN-SHOWN         VALUE 'Y'.          
               16  pi-mo-ben              pic s9(9)v99 comp-3.
               16  pi-max-moben           pic s9(7)v99 comp-3.
               16  pi-exp-dt               pic xx.
               16  pi-wsm-claim-nos occurs 13 pic x(7).
00653          16  FILLER                  PIC X(250).
00654                                                                   
00655      EJECT                                                        
                                           copy DFHBMSCA.
00656                                      COPY ELCAID.                 
00657  01  FILLER    REDEFINES DFHAID.                                  
00658      12  FILLER                      PIC X(8).                    
00659      12  PF-VALUES                   PIC X       OCCURS 24 TIMES. 

00661                                      COPY EL1503S.                

00677  LINKAGE SECTION.                                                 
00678  01  DFHCOMMAREA                     PIC X(1024).                 

00680                                      COPY ELCMSTR.                
00682                                      COPY ELCCNTL.                
00684                                      COPY ELCCERT.                
00684                                      COPY ELCCRTT.
00686                                      COPY ELCTRLR.
                                           COPY ERCPDEF.
                                           COPY ERCACCT.

00698  PROCEDURE DIVISION.                                              
00699                                                                   
           display ' ENTERING EL1503 '
00700      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
00701      MOVE '5'                    TO  DC-OPTION-CODE.              
00702      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
00703      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   
00704      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               
00705      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.               
00706                                                                   
00707      IF SAVE-DATE-YY > 70                                         
00708          MOVE 19                 TO  SAVE-DATE-CC                 
00709      ELSE                                                         
00710          MOVE 20                 TO  SAVE-DATE-CC.                
00711                                                                   
00712      IF EIBCALEN = 0                                              
00713          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00714                                                                   
00715      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00716                                                                   
00717      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         
00718      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           
00719                                                                   
00720      MOVE EIBTRMID               TO QID-TERM                      
00721                                     WS-QID-TERM.                  
00722                                                                   
00723      EXEC CICS HANDLE CONDITION                                   
00724          QIDERR   (1000-SHOW-CLAIM-HISTORY)                       
00725          MAPFAIL  (0100-FIRST-TIME-IN)                            
00726          NOTOPEN  (8500-FILE-NOTOPEN)                             
00727          PGMIDERR (9600-PGMID-ERROR)                              
00728          ERROR    (9990-ABEND)                                    
00729      END-EXEC.                                                    
00730                                                                   
00731      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           
00732          MOVE PI-CALLING-PROGRAM       TO RETURNED-FROM.          
00733                                                                   
00734      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00735          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00736              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00737              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00738              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00739              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00740              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00741              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00742              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00743              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00744          ELSE                                                     
00745              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00746              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00747              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00748              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00749              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00750              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00751              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00752              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
00753                                                                   
00754      IF RETURNED-FROM NOT = SPACES                                
              go to 0100-first-time-in.
00755 *        GO TO 0600-RECOVER-TEMP-STORAGE.                         
00756                                                                   
00757      IF EIBAID = DFHCLEAR                                         
00758          GO TO 9400-CLEAR.                                        
00759                                                                   
00760      IF PI-PROCESSOR-ID = 'LGXX'                                  
00761          NEXT SENTENCE                                            
00762      ELSE                                                         
00763          EXEC CICS READQ TS                                       
00764              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
00765              INTO    (SECURITY-CONTROL)                           
00766              LENGTH  (SC-COMM-LENGTH)                             
00767              ITEM    (SC-ITEM)                                    
00768          END-EXEC                                                 
00769              MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP      
00770              MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP       
00771              IF NOT DISPLAY-CAP                                   
00772                  MOVE 'READ'              TO  SM-READ             
00773                  PERFORM 9995-SECURITY-VIOLATION                  
00774                  MOVE ER-0070             TO  EMI-ERROR           
00775                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00776                  GO TO 8100-SEND-INITIAL-MAP.                     
00777                                                                   
00778      IF EIBTRNID = TRANS-ID                                       
00779          GO TO 0200-RECEIVE.                                      
00780                                                                   
00781  EJECT                                                            
00782  0100-FIRST-TIME-IN.                                              
           display ' made it to first time '
00783      MOVE LOW-VALUES             TO  EL150DO
00784                                      PI-PROGRAM-WORK-AREA
00786                                                                   
00787      MOVE 'Y'                    TO  PI-FIRST-TIME-SW
00788                                      WS-RECORDS-READ-SW
00789      MOVE 'F'                    TO  DIRECTION-SWITCH
00790      MOVE 1                      TO  PI-LINE-NO                   
00791                                      SUB-2                        
00792                                      SUB-3
00793      MOVE ZERO                   TO  WS-MAX-SUB                   
00803                                      SUB
                                           PI-MO-BEN
                                           pi-max-moben

00805      PERFORM 7400-DEL-TEMP-STOR-TABLE
                                       THRU 7400-EXIT

00811      EXEC CICS DELETEQ TS                                         
00812         QUEUE  (QID)
              RESP   (WS-RESPONSE)
00813      END-EXEC
00814                                                                   
00815      PERFORM 0900-BUILD-RELATED-TABLE
                                       THRU 0900-EXIT
00816                                                                   
00817      GO TO 1000-SHOW-CLAIM-HISTORY

           .
00820  0200-RECEIVE.                                                    
00821      MOVE 'B'                    TO  PASS-SWITCH.                 
00822      MOVE LOW-VALUES             TO  EL150DI.                     
00823                                                                   
00824      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00825          MOVE ER-0008            TO  EMI-ERROR                    
00826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00827          MOVE -1                 TO  ENTERPFL                     
00828          GO TO 8200-SEND-DATAONLY.                                
00829                                                                   
00830      EXEC CICS RECEIVE                                            
00831          MAP      (MAP-NAME)                                      
00832          MAPSET   (MAPSET-NAME)                                   
00833          INTO     (EL150DI)                                       
00834      END-EXEC.                                                    
00835                                                                   
00836      IF ENTERPFL = 0                                              
00837          GO TO 0300-CHECK-PFKEYS.                                 
00838                                                                   
00839      IF EIBAID NOT = DFHENTER                                     
00840          MOVE ER-0004            TO  EMI-ERROR                    
00841          GO TO 0320-INPUT-ERROR.                                  
00842                                                                   
00843      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)            
00844          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   
00845      ELSE                                                         
00846          MOVE ER-0029                TO  EMI-ERROR                
00847          GO TO 0320-INPUT-ERROR.                                  
00848                                                                   
00849  0300-CHECK-PFKEYS.                                               
00850      IF EIBAID = DFHPF23                                          
00851          GO TO 8810-PF23.                                         
00852                                                                   
00853      IF EIBAID = DFHPF24                                          
00854          GO TO 9200-RETURN-MAIN-MENU.                             
00855                                                                   
00856      IF EIBAID = DFHPF12                                          
00857          GO TO 9500-PF12.                                         
00858                                                                   
00859      IF EIBAID = DFHPF3                                           
00860          IF SELECTL > +0                                          
00861              GO TO 0500-CREATE-TEMP-STORAGE                       
00862          ELSE                                                     
00863              MOVE ER-0672        TO  EMI-ERROR                    
00864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00865              MOVE -1             TO  SELECTL                      
00866              GO TO 8200-SEND-DATAONLY.                            

021615     IF EIBAID = DFHPF4
021615         IF SELECTL > +0                                          
021615             GO TO 0500-CREATE-TEMP-STORAGE                       
021615         ELSE                                                     
021615             MOVE ER-0672        TO  EMI-ERROR                    
021615             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
021615             MOVE -1             TO  SELECTL                      
021615             GO TO 8200-SEND-DATAONLY.                            

021615     IF EIBAID = DFHPF5
021615         IF SELECTL > +0
021615             GO TO 0500-CREATE-TEMP-STORAGE
021615         ELSE
021615             MOVE ER-0672        TO  EMI-ERROR
021615             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021615             MOVE -1             TO  SELECTL
021615             GO TO 8200-SEND-DATAONLY.

00907      MOVE SPACES                 TO  ERRMSG1O.                    
00908                                                                   
00909      IF EIBAID = DFHPF1                                           
00910          MOVE 'F'                TO  DIRECTION-SWITCH             
00911          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT         
00912          GO TO 1000-SHOW-CLAIM-HISTORY.                           
00913                                                                   
00914      IF EIBAID = DFHPF2                                           
00915          MOVE 'B'                TO  DIRECTION-SWITCH             
00916          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT         
00917          GO TO 1000-SHOW-CLAIM-HISTORY.                           
00918                                                                   
00919      IF EIBAID = DFHENTER                                         
00920          GO TO 0330-EDIT-DATA.                                    
00921                                                                   
00922      MOVE ER-0029                TO EMI-ERROR.                    
00923                                                                   
00924  0320-INPUT-ERROR.                                                
00925      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00926                                                                   
00927      IF ENTERPFL = 0                                              
00928          MOVE -1                 TO ENTERPFL                      
00929      ELSE                                                         
00930          MOVE AL-UNBON           TO ENTERPFA                      
00931          MOVE -1                 TO ENTERPFL.                     
00932                                                                   
00933      GO TO 8200-SEND-DATAONLY.                                    
00934                                                                   
00935  0330-EDIT-DATA.                                                  
00936      IF NOT MODIFY-CAP                                            
00937          MOVE 'UPDATE'           TO  SM-READ                      
00938          PERFORM 9995-SECURITY-VIOLATION                          
00939          MOVE ER-0070            TO  EMI-ERROR                    
00940          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00941          GO TO 8100-SEND-INITIAL-MAP.                             
00942                                                                   
00951                                                                   
00971                                                                   
00972      MOVE 'F'                    TO  DIRECTION-SWITCH.            
00973      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            
00974      GO TO 1000-SHOW-CLAIM-HISTORY.                               
00975                                                                   
00976      EJECT                                                        
00977  0500-CREATE-TEMP-STORAGE.                                        
           display '**' selecti '**'
           if (selecti not numeric)
              or (selecti < 1)
              move 01 to selecti
           end-if
00978      MOVE EIBCPOSN               TO  PI-SAVE-CURSOR.              
00979      MOVE SPACES                 TO PI-FULL-SCREEN-IND.           
00980                                                                   
00981      EXEC CICS WRITEQ TS                                          
00982          QUEUE    (QID)                                           
00983          FROM     (PROGRAM-INTERFACE-BLOCK)                       
00984          LENGTH   (PI-COMM-LENGTH)                                
00985      END-EXEC.                                                    

01051      IF EIBAID = DFHPF3
               move pi-wsm-claim-nos (selecti) to pi-claim-no
01052          MOVE XCTL-150           TO  PGM-NAME                     
01053          MOVE 'EL150A'           TO  PI-MAP-NAME                  
01063          MOVE 'Y'                TO  PI-FIRST-TIME-SW             
01064          GO TO 9300-XCTL.                                         
01065                                                                   
021615     IF EIBAID = DFHPF4
021615         move pi-wsm-claim-nos (selecti) to pi-claim-no
021615         MOVE XCTL-1504          TO  PGM-NAME                     
021615         MOVE 'EL150E'           TO  PI-MAP-NAME                  
021615         MOVE 'Y'                TO  PI-FIRST-TIME-SW             
021615         GO TO 9300-XCTL.                                         
01065
100518     IF EIBAID = DFHPF5
100518         move pi-wsm-claim-nos (selecti) to pi-claim-no
100518         MOVE xctl-1285          TO  PGM-NAME
100518         MOVE 'EL128E'           TO  PI-MAP-NAME
100518         MOVE 'Y'                TO  PI-FIRST-TIME-SW
100518         GO TO 9300-XCTL.
01065                                                                   
01066      EJECT                                                        
01067  0600-RECOVER-TEMP-STORAGE.                                       
           display ' made it to 0600 '
01068      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.                 
01069                                                                   
01070      EXEC CICS HANDLE CONDITION                                   
01071          QIDERR   (0690-QIDERR)                                   
01072      END-EXEC.                                                    
01073                                                                   
01074      EXEC CICS READQ TS                                           
01075          QUEUE    (QID)                                           
01076          INTO     (PROGRAM-INTERFACE-BLOCK)                       
01077          LENGTH   (PI-COMM-LENGTH)                                
01078      END-EXEC.                                                    
01079                                                                   
01080      EXEC CICS DELETEQ TS                                         
01081          QUEUE   (QID)                                            
01082      END-EXEC.                                                    
01083                                                                   
01084      MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.      
01085      MOVE 'F'                    TO  DIRECTION-SWITCH.            
01086                                                                   
01087      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            
01088      GO TO 1000-SHOW-CLAIM-HISTORY.                               
01089                                                                   
01090  0690-QIDERR.                                                     
01091      MOVE ER-0033                TO  EMI-ERROR.                   
01092      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01093      MOVE LOW-VALUES             TO  EL150DO.                     
01094      MOVE -1                     TO  ENTERPFL.                    
01095      GO TO 8100-SEND-INITIAL-MAP.                                 
01096                                                                   
01097      EJECT                                                        
01098  0700-CREATE-TEMP-STORAGE.                                        
01099                                                                   
01100      EXEC CICS WRITEQ TS                                          
01101          QUEUE    (WS-TABLE-QID)                                  
01102          FROM     (WS-SORTED-TABLE)                               
01103          LENGTH   (WS-TABLE-LENGTH)                               
01104          ITEM     (WS-ITEM-COUNT)                                 
01105      END-EXEC.                                                    
01106                                                                   
01107  0700-EXIT.                                                       
01108      EXIT.                                                        
01109                                                                   
01110      EJECT                                                        
01111  0800-RECOVER-TEMP-STORAGE.                                       
01112                                                                   
01113      EXEC CICS HANDLE CONDITION                                   
01114          QIDERR   (0890-QIDERR)                                   
01115      END-EXEC.                                                    
01116                                                                   
01117      EXEC CICS READQ TS                                           
01118          QUEUE    (WS-TABLE-QID)                                  
01119          INTO     (WS-SORTED-TABLE)                               
01120          LENGTH   (WS-TABLE-LENGTH)                               
01121          ITEM     (WS-ITEM-COUNT)                                 
01122      END-EXEC.                                                    
01123                                                                   
01124      GO TO 0899-EXIT.                                             
01125                                                                   
01126  0890-QIDERR.                                                     
01127      MOVE ER-0033                TO  EMI-ERROR.                   
01128      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01129      MOVE LOW-VALUES             TO  EL150DO.                     
01130      MOVE -1                     TO  ENTERPFL.                    
01131      GO TO 8100-SEND-INITIAL-MAP.                                 
01132                                                                   
01133  0899-EXIT.                                                       
01134      EXIT.                                                        
01135                                                                   
01136      EJECT                                                        
01137  0900-BUILD-RELATED-TABLE.

          display ' made it to 0900 ' pi-carrier ' ' pi-grouping ' '
             pi-state ' ' pi-account ' ' pi-cert-no
     
           move spaces to ws-sorted-table ws-unsorted-table

           MOVE LOW-VALUES             TO ELCERT-KEY
           MOVE PI-COMPANY-CD          TO CERT-COMP-CD
           MOVE PI-CARRIER             TO CERT-CARRIER
           MOVE PI-GROUPING            TO CERT-GROUPING
           MOVE PI-STATE               TO CERT-STATE
           MOVE PI-ACCOUNT             TO CERT-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CERT-EFF-DT
           MOVE PI-CERT-NO             TO CERT-CERT-NO

           EXEC CICS READ
              DATASET   ('ELCERT')
              RIDFLD    (ELCERT-KEY)
              SET       (ADDRESS OF CERTIFICATE-MASTER)
              resp      (ws-response)
           END-EXEC

           IF NOT RESP-NORMAL
              display ' something went wrong ' pi-cert-no
           END-IF
           display ' good read on elcert '

           move cm-ah-benefit-amt      to pi-mo-ben

           move low-values             to pi-exp-dt
           if cm-ah-loan-expire-dt > pi-exp-dt
              move cm-ah-loan-expire-dt to pi-exp-dt
           end-if
           if cm-lf-loan-expire-dt > pi-exp-dt
              move cm-lf-loan-expire-dt to pi-exp-dt
           end-if

           MOVE ' '                    TO WS-PDEF-RECORD-SW
           perform 2100-match-eracct   thru 2100-exit
      *    if (acct-found)
      *       and (am-dcc-product-code not = spaces)
      *       perform 3000-get-ddf-limits
      *                                thru 3000-exit
      *    end-if

           MOVE CM-CONTROL-PRIMARY     TO ELCRTT-KEY
           MOVE 'B'                    TO CRTT-REC-TYPE

           EXEC CICS READ
              DATASET   ('ELCRTT')
              RIDFLD    (ELCRTT-KEY)
              SET       (ADDRESS OF CERTIFICATE-TRAILERS)
              resp      (ws-response)
           END-EXEC

           if not resp-normal
              display ' no trlrs ' pi-cert-no
              go to 0900-exit
           end-if

           display ' good read on elcrtt ' 

           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cs-claim-no (s1) = spaces)
              move cs-claim-no (s1)    to ws-claim-no (s1)
              move cs-claim-type (s1)  to ws-clm-type (s1)
              if cs-insured-type (s1) = 'C'
                 move '2'              to ws-ins-type (s1)
              else
                 move '1'              to ws-ins-type (s1)
              end-if
              move cs-benefit-period (s1)
                                       to ws-ben-per (s1)
      *       move cs-days-paid (s1)   to ws-days-paid (s1)
              move cs-total-paid (s1)  to ws-total-paid (s1)
              move ' '                 to ws-sorted-sw (s1)

              move cs-company-cd       to mstr-comp-cd
              move cs-carrier          to mstr-carrier
              move cs-claim-no (s1)    to mstr-claim-no
              move cs-cert-no          to mstr-cert-no
              exec cics read
                 dataset        ('ELMSTR')
                 ridfld         (elmstr-key)
                 set            (address of claim-master)
                 resp           (ws-response)
              end-exec
              if resp-normal
                 move cl-incurred-dt   to ws-inc-dt (s1)
                 move cl-paid-thru-dt  to ws-pd-thru-dt (s1)
                 move cl-claim-status  to ws-status (s1)
                 if cl-critical-period not numeric
                    move zeros to cl-critical-period
                 end-if
                 move cl-critical-period
                                       to ws-max-bens (s1)
                 if cl-critical-period = zeros
100518              if cl-claim-type = 'L' or 'P' OR 'O'
                       move 01         to ws-max-bens (s1)
                    else
                       move cm-ah-orig-term
                                       to ws-max-bens (s1)
                    end-if
                 end-if
                 if cl-denial-type = '1' or '2' or '3' or '4'
                    move 'D'           to ws-status (s1)
                 end-if
080322           move cl-insured-birth-dt
080322                                  to dc-bin-date-1
080322           move cl-incurred-dt    to dc-bin-date-2
080322           move '1'               to dc-option-code
080322           PERFORM 9700-LINK-DATE-CONVERT
080322                                  THRU 9700-EXIT
080322           compute ws-att-age =
080322               dc-elapsed-months / 12
080322           move zeros to dc-elapsed-months dc-elapsed-days

                 MOVE ' '                    TO WS-PDEF-RECORD-SW
                 if (acct-found)
                    and (am-dcc-product-code not = spaces)
                    perform 3000-get-ddf-limits
                                       thru 3000-exit
                 end-if
                 if pdef-found
                    perform varying p1 from +1 by +1 until
                       (pd-prod-code (p1) = cl-claim-type
080322                 and PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
080322                 or (p1 > +11)
                    end-perform
080322              if p1 < +12
100518                 if cl-claim-type = 'L' or 'P' OR 'O'
                          if (pd-max-amt (p1) not = zeros)
                             and (pd-max-amt (p1) < cm-lf-benefit-amt)
                             move pd-max-amt (p1)
                                       to ws-max-moben (s1)
                          else
                             move cm-lf-benefit-amt
                                       to ws-max-moben (s1)
                          end-if
                       else
100314                    if pd-ben-pct (p1) not numeric
100314                       move zeros   to pd-ben-pct (p1)
100314                    end-if
100314                    if pd-ben-pct (p1) = zeros
100314                       move +1      to ws-work-ben-pct
100314                    else
100314                       move pd-ben-pct (p1)
100314                                    to ws-work-ben-pct
100314                    end-if
100314                    compute ws-max-moben (s1) =
100314                       cm-ah-benefit-amt * ws-work-ben-pct
100314                    if (pd-max-amt (p1) not = zeros)
100314                       and (pd-max-amt (p1) < ws-max-moben (s1))
100314                       move pd-max-amt (p1)
100314                                 to ws-max-moben (s1)
100314                    end-if
                       end-if
      *                if (pd-max-amt (p1) not = zeros)
      *                   and (pd-max-amt (p1) < cm-ah-benefit-amt)
      *                   move pd-max-amt (p1) to ws-max-moben (s1)
      *                else
      *                   move cm-ah-benefit-amt to ws-max-moben (s1)
      *                end-if
                       move pd-exclusion-period-days (p1)
                                       to ws-excl-per (s1)
                       move pd-coverage-ends-mos (p1)
                                       to ws-cov-ends (s1)
                       move pd-accident-only-mos (p1)
                                       to ws-acc-per (s1)
                       move pd-max-extension (p1)
                                       to ws-max-exten (s1)
                       evaluate true
                          when pd-recurring-yn (p1) = 'N'
                             move 00   to ws-rec-mos (s1)
                          when pd-recurring-yn (p1) = 'Y'
                             move 99   to ws-rec-mos (s1)
                          when pd-rec-crit-period (p1) numeric
                             move pd-rec-crit-period (p1)
                                       to ws-rec-mos (s1)
                          when other
                             move zeros to ws-rec-mos (s1)
                       end-evaluate
                       if ws-rec-mos (s1) = zeros
                          move 01      to ws-rec-mos (s1)
                       end-if
                       display ' pdef found ' ws-excl-per (s1)
                    end-if
                 else
100518              if cl-claim-type not = 'L' and 'P' AND 'O'
                       move cm-ah-benefit-amt
                                       to ws-max-moben (s1)
                    else
                       move zeros      to ws-max-moben (s1)
                    end-if
                    move zeros         to ws-excl-per (s1)
                                          ws-max-exten (s1)
                                          ws-acc-per  (s1)
                    move 999           to ws-cov-ends (s1)
                    move 01            to ws-rec-mos  (s1)
                 end-if
              end-if
           end-perform
           DISPLAY ' JUST BUILT TABLE '

           move high-values            to ws-key (s1)
           compute ws-max-sub = s1 - +1
           move ws-max-sub             to pi-max-sub
                                          ws-cntr

           move +1                     to s2
           move +0                     to s3
           perform until (ws-cntr = zero) or (s3 > 700)
              move +0 to ws-hold-s1
              move high-values to ws-hold-key
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
                 move ws-status     (s1) to ws-srtd-status     (s2)
                 move ws-max-bens   (s1) to ws-srtd-max-bens   (s2)
                 move ws-excl-per   (s1) to ws-srtd-excl-per   (s2)
                 move ws-cov-ends   (s1) to ws-srtd-cov-ends   (s2)
                 move ws-acc-per    (s1) to ws-srtd-acc-per    (s2)
                 move ws-rec-mos    (s1) to ws-srtd-rec-mos    (s2)
                 move ws-max-exten  (s1) to ws-srtd-max-exten  (s2)
                 move ws-pd-thru-dt (s1) to ws-srtd-pd-thru-dt (s2)
                 move ws-claim-no   (s1) to ws-srtd-claim-no   (s2)
                 move ws-max-moben  (s1) to ws-srtd-max-moben  (s2)
                 move ws-total-paid (s1) to ws-srtd-total-paid (s2)
      *          move ws-rem-bens   (s1) to ws-srtd-rem-bens   (s2)
                 move 'Y'                to ws-sorted-sw       (s1)
                 subtract 1 from ws-cntr
                 add +1 to s2
              end-if
              add 1 to s3
           end-perform

           perform 0700-CREATE-TEMP-STORAGE
                                       thru 0700-exit

           display ' just sorted table '

           .
01343  0900-EXIT.
01344      EXIT.                                                        
01345                                                                   
01346      EJECT                                                        
01347  1000-SHOW-CLAIM-HISTORY.                                         
           display ' made it to 1000 '

01351      PERFORM 2070-INIT-SCREEN-AREA
                                       THRU 2070-EXIT

01349 *    IF DIRECTION-SWITCH = 'F'                                    
01350 *       IF FIRST-TIME                                            
01351 *          PERFORM 2070-INIT-SCREEN-AREA
      *                                  THRU 2070-EXIT
01352 *          MOVE 'N'                TO PI-FIRST-TIME-SW         
01353 *          MOVE +1                 TO DISPLAY-CNT              
01354 *                                     SUB-1                    
01355 *                                     SUB-2                    
01356 *       ELSE                                                     
01357 *          MOVE PI-TRLR-SUB (8)    TO  SUB-2                    
01358 *          ADD +1                  TO  SUB-2                    
01359 *          IF SUB-2 > PI-MAX-SUB                                
01360 *             GO TO 8200-SEND-DATAONLY                         
01361 *          ELSE                                                 
01362 *             PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT     
01363 *             MOVE +1                 TO  DISPLAY-CNT          
01364 *                                            SUB-1                
01365 *                                            PI-LINE-NO           
01366 *             IF SUB-2 = +9                                    
01367 *                IF WS-SRTD-CERT (SUB-2) = LOW-VALUES          
01368 *                   MOVE +1             TO  SUB-2.               
01369 *                                                                 
01370 *    IF DIRECTION-SWITCH = 'B'                                    
01371 *        IF FIRST-TIME                                            
01372 *            GO TO 8200-SEND-DATAONLY                             
01373 *        ELSE                                                     
01374 *            PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT         
01375 *            MOVE PI-TRLR-SUB (1)    TO  SUB-2                    
01376 *            IF SUB-2 > +8                                        
01377 *                SUBTRACT +1        FROM SUB-2                    
01378 *                MOVE +15            TO  DISPLAY-CNT              
01379 *                MOVE +8             TO  SUB-1                    
01380 *                                        PI-LINE-NO               
01381 *            ELSE                                                 
01382 *                MOVE 'F'            TO  DIRECTION-SWITCH         
01383 *                MOVE +1             TO  DISPLAY-CNT              
01384 *                                        SUB-1                    
01385 *                                        SUB-2                    
01386 *                                         PI-LINE-NO.              
           .
01388  1010-BUILD-HISTORY-SCREEN.

020816     if pi-company-id not = 'DCC' and 'VPP'
              go to 2000-build-history-screen
           end-if

           move spaces                 to ws-prev-clm-type
                                          ws-prev-ins-type
           move zeros                  to ws-prev-ben-per
                                          ws-accum-days
                                          ws-accum-amt
                                          ws-accum-pd-bens

           move +1                     to m1
           perform varying s1 from +1 by +1 until
              s1 > ws-max-sub
              if (ws-srtd-clm-type (s1) = ws-prev-clm-type)
                 and (ws-srtd-ben-per (s1) = ws-prev-ben-per)
                 and (ws-srtd-ins-type (s1) not = ws-prev-ins-type)
                 move zeros            to ws-accum-days
                                          ws-accum-amt
                                          ws-accum-pd-bens
              end-if
              move ws-srtd-ins-type (s1) to ws-prev-ins-type
              if (ws-srtd-clm-type (s1) = ws-prev-clm-type)
                 and (ws-srtd-ben-per (s1) not = ws-prev-ben-per)
                 move zeros            to ws-accum-days
                                          ws-accum-amt
                                          ws-accum-pd-bens
              end-if
              move ws-srtd-ben-per (s1) to ws-prev-ben-per
              if ws-srtd-clm-type (s1) not = ws-prev-clm-type
                 perform 1020-set-new-head
                                       thru 1020-exit
                 move ws-srtd-clm-type (s1)
                                       to ws-prev-clm-type              
                 move WS-MAP-OUTPUT (m1) to replineo (m1)
                 move dfhyello           to replinec (m1)
                 add +1                to m1
              end-if
              add 1 to ws-map-line-no
              move ws-map-line-no      to wsm-line-no (m1)
              move ws-srtd-ben-per (s1)  to wsm-ben-per (m1)
              if ws-srtd-ins-type (s1) = '1'
                 move 'PRIM'             to wsm-ins-type (m1)
              else
                 move 'COBO'             to wsm-ins-type (m1)
              end-if
              evaluate ws-srtd-status (s1)
                 when 'C'
                    MOVE 'CLOSED'        TO WSM-STATUS (M1)
                 WHEN 'D'
                    MOVE 'DENIED'        TO WSM-STATUS (M1)
                 WHEN 'O'
                    MOVE 'OPEN'          TO WSM-STATUS (M1)
                 WHEN OTHER
                    MOVE 'OTHER'         TO WSM-STATUS (M1)
              END-EVALUATE
              move ws-srtd-claim-no (s1) to wsm-claim-no (m1)
                                            pi-wsm-claim-nos (s1)
              move ws-srtd-total-paid (s1) to wsm-total-paid (m1)

              compute wsm-max-bens (m1) =
                 ws-srtd-max-bens (s1) - ws-accum-pd-bens

              display ' max bens 1 ' wsm-max-bens (m1) ' '
                 ws-srtd-max-bens (s1) ' ' ws-accum-pd-bens

              move zeros to wk1 wk2
              if ws-srtd-max-moben (s1) not = zeros
                 compute ws-pd-bens rounded =
                    ws-srtd-total-paid (s1) / ws-srtd-max-moben (s1)
100518           if ws-srtd-clm-type (s1) not = 'L' and 'P' AND 'O'
                    divide ws-srtd-total-paid (s1) by
                       ws-srtd-max-moben(s1) giving wk1
                       remainder wk2
                 end-if
              else
                 move zeros            to ws-pd-bens
              end-if
              if (ws-pd-bens = zeros)
100518           and (ws-srtd-clm-type (s1) = 'L' OR 'P' OR 'O')
                 and (ws-srtd-total-paid (s1) > zeros)
                 move 1                to ws-pd-bens
              end-if
              move ws-pd-bens          to wsm-pd-bens (m1)
              if wk2 not = zeros
                 move '*'              to wsm-part (m1)
              end-if
      *       compute ws-accum-days =
      *          ws-accum-days + ws-srtd-days-paid (s1)
              compute ws-accum-amt =
                 ws-accum-amt + ws-srtd-total-paid (s1)
              display ' compute a pd bens ' m1 ' '
                 ws-accum-amt ' ' ws-srtd-max-moben (s1)
              if ws-srtd-max-moben (s1) not = zeros
                 compute ws-accum-pd-bens rounded =
                    ws-accum-amt / ws-srtd-max-moben (s1)
              else
                 move zeros            to ws-accum-pd-bens
              end-if
              if (ws-accum-pd-bens = zeros)
100518           and (ws-srtd-clm-type (s1) = 'L' OR 'P' OR 'O')
                 and (ws-srtd-total-paid (s1) > zeros)
                 move 1                to ws-accum-pd-bens
              end-if
              display ' compute rem bens ' m1 ' '
                 ws-srtd-max-bens (s1) ' ' ws-accum-pd-bens
              compute wsm-rem-bens (m1) =
                 ws-srtd-max-bens (s1) - ws-accum-pd-bens
              perform 1030-find-qualify thru 1030-exit

              MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
              MOVE ' '                    TO DC-OPTION-CODE
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              IF NO-CONVERSION-ERROR                                       
                 MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
              ELSE                                                         
                 MOVE SPACES              TO wsm-inc-date (m1)
              END-IF
              MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
              MOVE ' '                     TO DC-OPTION-CODE
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              IF NO-CONVERSION-ERROR                                       
                 MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
              ELSE                                                         
                 MOVE SPACES              TO wsm-pd-thru-dt (m1)
              END-IF
              move WS-MAP-OUTPUT (m1)  to replineo (m1)
              add +1                   to m1
           end-perform

01395      MOVE -1                     TO  SELECTL.                     
01396                                                                   
01397      IF RECORDS-READ                                              
01398          MOVE 'N'                TO  WS-RECORDS-READ-SW           
01399          GO TO 8100-SEND-INITIAL-MAP                              
01400      ELSE                                                         
01401          GO TO 8200-SEND-DATAONLY.                                
01402                                                                   
01403      EJECT                                                        

       1020-set-new-head.
           move zeros                  to ws-accum-days
                                          ws-accum-amt
                                          ws-accum-pd-bens
           evaluate ws-srtd-clm-type (s1)
              when 'A'
                 MOVE 'A&H'            to ws-cov-type
              when 'F'
052614           MOVE 'FAM '           to ws-cov-type
080322        when 'B'
080322           move 'BRV  '          to ws-cov-type
080322        when 'H'
080322           move 'HOSP '          to ws-cov-type
100518        when 'O'
100518           MOVE 'OTH '           to ws-cov-type
              when 'I'
                 move ' IU '           to ws-cov-type
              when 'L'
                 move 'LIFE'           to ws-cov-type
              when other
                 move ws-srtd-clm-type (s1)
                                       to ws-cov-type
           end-evaluate


           string '  ' ws-cov-type ' ExPer ' ws-srtd-excl-per (s1)
              ' CovEnd ' ws-srtd-cov-ends (s1)
              ' MaxBens ' ws-srtd-max-bens (s1)
              ' Recurring ' ws-srtd-rec-mos (s1)
              delimited by size into ws-map-output (m1)
           end-string

           .
       1020-exit.
           exit.

       1030-find-qualify.
           if not pdef-found
              go to 1030-exit
           end-if

           display ' max bens 2 ' wsm-max-bens (m1) ' '
              ws-srtd-max-bens (s1) ' ' ws-accum-pd-bens

           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              display ' good date convert '
              MOVE DC-ELAPSED-MONTHS   TO WS-MONTHS-BETWEEN
              IF DC-ELAPSED-DAYS > 1
                 ADD 1 TO WS-MONTHS-BETWEEN
              END-IF
           ELSE
              MOVE ZEROS               TO WS-MONTHS-BETWEEN
           END-IF

           display ' display 3 ' ws-srtd-excl-per (s1)
              ' ' ws-srtd-cov-ends (s1) ' ' ws-months-between
   
           evaluate true
              when (ws-srtd-excl-per (s1) not = zeros)
                 and (ws-months-between <= ws-srtd-excl-per (s1))
                 display ' zero 1 '
                 move zeros to wsm-max-bens (m1)
                               wsm-rem-bens (m1)
              when (ws-srtd-cov-ends (s1) not = zeros)
                 and (ws-months-between > ws-srtd-cov-ends (s1))
                 display ' zero 2 '
                 move zeros to wsm-max-bens (m1)
           end-evaluate
           if ws-srtd-rec-mos (s1) < ws-srtd-ben-per (s1)
              move zeros               to wsm-rem-bens (m1)
           end-if

           .
       1030-exit.
           exit.

01388  2000-BUILD-HISTORY-SCREEN.

           move spaces                 to ws-prev-clm-type
                                          ws-prev-ins-type
           move zeros                  to ws-prev-ben-per
                                          ws-accum-days
                                          ws-accum-amt
                                          ws-accum-pd-bens

           move +1                     to m1
           perform varying s1 from +1 by +1 until
              s1 > ws-max-sub
              if ws-srtd-clm-type (s1) not = ws-prev-clm-type
                 perform 1020-set-new-head
                                       thru 1020-exit
                 move ws-srtd-clm-type (s1)
                                       to ws-prev-clm-type              
                 move WS-MAP-OUTPUT (m1) to replineo (m1)
                 move dfhyello           to replinec (m1)
                 add +1                to m1
              end-if
              add 1 to ws-map-line-no
              move ws-map-line-no      to wsm-line-no (m1)
              move ws-srtd-ben-per (s1)  to wsm-ben-per (m1)
              if ws-srtd-ins-type (s1) = '1'
                 move 'PRIM'             to wsm-ins-type (m1)
              else
                 move 'COBO'             to wsm-ins-type (m1)
              end-if
              evaluate ws-srtd-status (s1)
                 when 'C'
                    MOVE 'CLOSED'        TO WSM-STATUS (M1)
                 WHEN 'D'
                    MOVE 'DENIED'        TO WSM-STATUS (M1)
                 WHEN 'O'
                    MOVE 'OPEN'          TO WSM-STATUS (M1)
                 WHEN OTHER
                    MOVE 'OTHER'         TO WSM-STATUS (M1)
              END-EVALUATE
              move ws-srtd-claim-no (s1) to wsm-claim-no (m1)
                                            pi-wsm-claim-nos (s1)
              move ws-srtd-total-paid (s1) to wsm-total-paid (m1)

              compute wsm-max-bens (m1) =
                 ws-srtd-max-bens (s1) - ws-accum-pd-bens

              display ' max bens 1 ' wsm-max-bens (m1) ' '
                 ws-srtd-max-bens (s1) ' ' ws-accum-pd-bens

              move zeros to wk1 wk2
              if ws-srtd-max-moben (s1) not = zeros
                 compute ws-pd-bens rounded =
                    ws-srtd-total-paid (s1) / ws-srtd-max-moben (s1)
100518           if ws-srtd-clm-type (s1) not = 'L' and 'P' AND 'O'
                    divide ws-srtd-total-paid (s1) by
                       ws-srtd-max-moben(s1) giving wk1
                       remainder wk2
                 end-if
              else
                 move zeros            to ws-pd-bens
              end-if
              if (ws-pd-bens = zeros)
100518           and (ws-srtd-clm-type (s1) = 'L' OR 'P' OR 'O')
                 and (ws-srtd-total-paid (s1) > zeros)
                 move 1                to ws-pd-bens
              end-if
              move ws-pd-bens          to wsm-pd-bens (m1)
              if wk2 not = zeros
                 move '*'              to wsm-part (m1)
              end-if
              compute ws-accum-amt =
                 ws-accum-amt + ws-srtd-total-paid (s1)
              display ' compute a pd bens ' m1 ' '
                 ws-accum-amt ' ' ws-srtd-max-moben (s1)
              if ws-srtd-max-moben (s1) not = zeros
                 compute ws-accum-pd-bens rounded =
                    ws-accum-amt / ws-srtd-max-moben (s1)
              else
                 move zeros            to ws-accum-pd-bens
              end-if
              if (ws-accum-pd-bens = zeros)
100518           and (ws-srtd-clm-type (s1) = 'L' OR 'P' OR 'O')
                 and (ws-srtd-total-paid (s1) > zeros)
                 move 1                to ws-accum-pd-bens
              end-if
              display ' compute rem bens ' m1 ' '
                 ws-srtd-max-bens (s1) ' ' ws-accum-pd-bens
              compute wsm-rem-bens (m1) =
                 ws-srtd-max-bens (s1) - ws-accum-pd-bens

              MOVE ws-srtd-inc-dt (s1)    TO DC-BIN-DATE-1
              MOVE ' '                    TO DC-OPTION-CODE
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              IF NO-CONVERSION-ERROR                                       
                 MOVE DC-GREG-DATE-A-EDIT TO wsm-inc-date (m1)
              ELSE                                                         
                 MOVE SPACES              TO wsm-inc-date (m1)
              END-IF
              MOVE ws-srtd-pd-thru-dt (s1) TO DC-BIN-DATE-1
              MOVE ' '                     TO DC-OPTION-CODE
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              IF NO-CONVERSION-ERROR                                       
                 MOVE DC-GREG-DATE-A-EDIT TO wsm-pd-thru-dt (m1)
              ELSE                                                         
                 MOVE SPACES              TO wsm-pd-thru-dt (m1)
              END-IF
              move WS-MAP-OUTPUT (m1)  to replineo (m1)
              add +1                   to m1
           end-perform

01395      MOVE -1                     TO  SELECTL.                     
01396                                                                   
01397      IF RECORDS-READ                                              
01398          MOVE 'N'                TO  WS-RECORDS-READ-SW           
01399          GO TO 8100-SEND-INITIAL-MAP                              
01400      ELSE                                                         
01401          GO TO 8200-SEND-DATAONLY.                                

01434  2070-INIT-SCREEN-AREA.                                           

01436      MOVE SPACES                 TO TEXT-WORK-AREAS
           MOVE +0                     TO M1
           perform varying m1 from +1 by +1 until m1 > +13
              move spaces              to replineo (m1)
           end-perform

           .
01459  2070-EXIT.                                                       
01460      EXIT.

       2100-MATCH-ERACCT.
                                                               
           move spaces                 to ws-eracct-sw
           MOVE cm-control-primary     to eracct-key
           move low-values             to acct-filler

           EXEC CICS STARTBR                                    
               DATASET('ERACCT')                                
               RIDFLD (ERACCT-KEY)                              
               GTEQ
               resp (ws-response)
           END-EXEC

           if not resp-normal
              go to 2100-exit
           end-if

           .                                                                
       2100-continue.
                                                                
           EXEC CICS READNEXT                                   
               DATASET('ERACCT')                                
               SET    (ADDRESS OF ACCOUNT-MASTER)               
               RIDFLD (ERACCT-KEY)                              
               resp (ws-response)
           END-EXEC
                                                                
           if not resp-normal
              go to 2100-exit
           end-if

           if am-control-primary (1:20) not =
                                       cm-control-primary (1:20)
              go to 2100-exit
           end-if

           if (cm-cert-eff-dt >= am-effective-dt)
              and (cm-cert-eff-dt < am-expiration-dt)
              set acct-found to true
           else
              if cm-cert-eff-dt = am-expiration-dt
                 go to 2100-continue
              end-if
           end-if

           .                                                                
       2100-exit.
           exit.

       3000-GET-DDF-LIMITS.
      
           if cm-clp-state = spaces or low-values or zeros
              move cm-state            to cm-clp-state
           end-if

           MOVE PI-COMPANY-CD          TO ERPDEF-KEY
           MOVE CM-clp-state           TO ERPDEF-STATE
           MOVE am-dcc-product-code    TO ERPDEF-PROD-CD

100518     if (cl-claim-type = 'L' or 'P' OR 'O')
              and (cm-lf-benefit-cd not = '00' and '  ' and 'DD')
              move 'L'                 to erpdef-ben-type
              move cm-lf-benefit-cd    to erpdef-ben-code
           else
              MOVE 'A'                 TO ERPDEF-BEN-TYPE
              MOVE cm-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
           end-if

           MOVE cm-CERT-EFF-DT         TO ERPDEF-EXP-DT
      
           DISPLAY ' MADE 3000 ' ERPDEF-KEY (2:15)
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
      
           EXEC CICS STARTBR
               DATASET  ('ERPDEF')
               RIDFLD   (ERPDEF-KEY)
               GTEQ
               RESP     (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
              GO TO 3000-EXIT
           END-IF
      
           .
       3000-READNEXT.
      
           EXEC CICS READNEXT
              DATASET  ('ERPDEF')
              SET      (ADDRESS OF PRODUCT-MASTER)
              RIDFLD   (ERPDEF-KEY)
              RESP     (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
              GO TO 3000-ENDBR
           END-IF
      
           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
                 display ' setting pdef found to true '
              ELSE
                 GO TO 3000-READNEXT
              END-IF
           ELSE
              GO TO 3000-ENDBR
           END-IF

           .
       3000-ENDBR.

           EXEC CICS ENDBR
              DATASET  ('ERPDEF')
           END-EXEC

           .
       3000-EXIT.
           EXIT.

03715  7400-DEL-TEMP-STOR-TABLE.                                        
03716                                                                   
03717      EXEC CICS HANDLE CONDITION                                   
03718          QIDERR   (7400-EXIT)                                     
03719      END-EXEC.                                                    
03720                                                                   
03721      EXEC CICS DELETEQ TS                                         
03722          QUEUE    (WS-TABLE-QID)                                  
03723      END-EXEC.                                                    
03724                                                                   
03725  7400-EXIT.                                                       
03726      EXIT.                                                        
03727                                                                   
03878  8000-LOAD-ERROR-MESSAGES.                                        
03879      IF EMI-NO-ERRORS                                             
03880          GO TO 8000-EXIT.                                         
03881                                                                   
03882      IF EMI-NUMBER-OF-LINES = 1                                   
03883          MOVE EMI-LINE1          TO  ERRMSG1O                     
03884          GO TO 8000-EXIT.                                         
03885                                                                   
03886      MOVE EMI-LINE1              TO  ERRMSG1O.                    
03887                                                                   
03888  8000-EXIT.                                                       
03889      EXIT.                                                        
03890                                                                   
03891  8100-SEND-INITIAL-MAP.                                           

           display ' made it to 8100 '
03897      MOVE SAVE-DATE              TO RUNDTEO.                     
03898      MOVE EIBTIME                TO TIME-IN.                     
03899      MOVE TIME-OUT               TO RUNTIMEO.                    
03900      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT
           MOVE PI-COMPANY-ID          TO CMPNYIDO
           MOVE PI-PROCESSOR-ID        TO USERIDO
           MOVE PI-CARRIER             TO CARRO
           MOVE PI-GROUPING            TO GROUPO
           MOVE PI-STATE               TO STATEO
           MOVE PI-ACCOUNT             TO ACCTO
           MOVE PI-CERT-NO             TO CERTNOO
           MOVE PI-MO-BEN              TO MOBENO

01536      MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1
01537      MOVE ' '                    TO  DC-OPTION-CODE
01538      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
01539      IF NO-CONVERSION-ERROR                                       
01540         MOVE DC-GREG-DATE-A-EDIT TO EFFDTEO
01541      ELSE                                                         
01542         MOVE SPACES              TO EFFDTEO
           END-IF

01536      MOVE PI-EXP-DT              TO  DC-BIN-DATE-1
01537      MOVE ' '                    TO  DC-OPTION-CODE
01538      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
01539      IF NO-CONVERSION-ERROR                                       
01540         MOVE DC-GREG-DATE-A-EDIT TO EXPDTEO
01541      ELSE                                                         
01542         MOVE SPACES              TO EXPDTEO
           END-IF

03912      EXEC CICS SEND                                               
03913          MAP      (MAP-NAME)                                      
03914          MAPSET   (MAPSET-NAME)                                   
03915          FROM     (EL150DO)                                       
03916          ERASE                                                    
03917          CURSOR                                                   
03918      END-EXEC.                                                    
03919                                                                   
03920      GO TO 9100-RETURN-TRAN.                                      
03921                                                                   
03922  8200-SEND-DATAONLY.                                              
           display ' made it to 8200 '

03927      MOVE SAVE-DATE              TO  RUNDTEO.                     
03928      MOVE EIBTIME                TO  TIME-IN.                     
03929      MOVE TIME-OUT               TO  RUNTIMEO.                    
03930      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             

03945      EXEC CICS SEND                                               
03946          MAP      (MAP-NAME)                                      
03947          MAPSET   (MAPSET-NAME)                                   
03948          FROM     (EL150DO)                                       
03949          DATAONLY                                                 
03950          CURSOR                                                   
03951      END-EXEC.                                                    
03952                                                                   
03953      GO TO 9100-RETURN-TRAN.                                      
03954                                                                   
03955  8300-SEND-TEXT.                                                  
03956      EXEC CICS SEND TEXT                                          
03957          FROM     (LOGOFF-TEXT)                                   
03958          LENGTH   (LOGOFF-LENGTH)                                 
03959          ERASE                                                    
03960          FREEKB                                                   
03961      END-EXEC.                                                    
03962                                                                   
03963      EXEC CICS RETURN                                             
03964      END-EXEC.                                                    
03965                                                                   
03966  8400-NOT-FOUND.                                                  
03967      IF FILE-SWITCH = 'BENE'                                      
03968          MOVE ER-0282            TO  EMI-ERROR.                   
03969                                                                   
03970      MOVE -1                     TO  SELECTL.                     
03971      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03972                                                                   
03973      IF PASS-SWITCH = 'A'                                         
03974          GO TO 8100-SEND-INITIAL-MAP                              
03975      ELSE                                                         
03976          GO TO 8200-SEND-DATAONLY.                                
03977                                                                   
03978  8500-FILE-NOTOPEN.                                               
03979                                                                   
03982                                                                   
03983      IF FILE-SWITCH = 'CERT'                                      
03984          MOVE ER-0169            TO  EMI-ERROR.                   
03985                                                                   
03991                                                                   
03992      IF FILE-SWITCH = 'MSTR'                                      
03993          MOVE ER-0154            TO  EMI-ERROR.                   
03994                                                                   
04000                                                                   
04001      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
04002                                                                   
04003      MOVE -1                     TO  SELECTL.                     
04004                                                                   
04005      IF PASS-SWITCH = 'A'                                         
04006          GO TO 8100-SEND-INITIAL-MAP                              
04007      ELSE                                                         
04008          GO TO 8200-SEND-DATAONLY.                                
04009                                                                   
04010  8800-UNAUTHORIZED-ACCESS.                                        
04011      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  
04012      GO TO 8300-SEND-TEXT.                                        
04013                                                                   
04014  8810-PF23.                                                       
04015      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
04016      MOVE XCTL-005               TO  PGM-NAME.                    
04017      GO TO 9300-XCTL.                                             
04018                                                                   
04019  9100-RETURN-TRAN.                                                
04020      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
04021      MOVE '150D'                 TO  PI-CURRENT-SCREEN-NO.        
04022                                                                   
04023      EXEC CICS RETURN                                             
04024          TRANSID    (TRANS-ID)                                    
04025          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
04026          LENGTH     (PI-COMM-LENGTH)                              
04027      END-EXEC.                                                    
04028                                                                   
04029  9200-RETURN-MAIN-MENU.                                           
04030      MOVE XCTL-126               TO PGM-NAME.                     
04031      GO TO 9300-XCTL.                                             
04032                                                                   
04033  9300-XCTL.                                                       
04034      EXEC CICS HANDLE CONDITION                                   
04035          PGMIDERR   (9350-NOT-FOUND)                              
04036      END-EXEC.                                                    
04037                                                                   
04038      EXEC CICS XCTL                                               
04039          PROGRAM    (PGM-NAME)                                    
04040          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
04041          LENGTH     (PI-COMM-LENGTH)                              
04042      END-EXEC.                                                    
04043                                                                   
04044  9350-NOT-FOUND.                                                  
04045      MOVE ER-0923                TO EMI-ERROR.                    
04046      MOVE -1                     TO SELECTL.                      
04047      PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.               
04050      GO TO 8200-SEND-DATAONLY.                                    
04051                                                                   
04052  9400-CLEAR.                                                      
04053      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    
04054      GO TO 9300-XCTL.                                             
04055                                                                   
04056  9500-PF12.                                                       
04057      MOVE XCTL-010               TO  PGM-NAME.                    
04058      GO TO 9300-XCTL.                                             
04059                                                                   
04060  9600-PGMID-ERROR.                                                
04061      EXEC CICS HANDLE CONDITION                                   
04062          PGMIDERR   (8300-SEND-TEXT)                              
04063      END-EXEC.                                                    
04064                                                                   
04065      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          
04066      MOVE ' '                    TO  PI-ENTRY-CD-1.               
04067      MOVE XCTL-005               TO  PGM-NAME.                    
04068      MOVE PGM-NAME               TO  LOGOFF-PGM.                  
04069      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
04070      GO TO 9300-XCTL.                                             
04071                                                                   
04072  9700-LINK-DATE-CONVERT.                                          
04073      MOVE LINK-ELDATCV           TO PGM-NAME.                     
04074                                                                   
04075      EXEC CICS LINK                                               
04076          PROGRAM    (PGM-NAME)                                    
04077          COMMAREA   (DATE-CONVERSION-DATA)                        
04078          LENGTH     (DC-COMM-LENGTH)                              
04079      END-EXEC.                                                    
04080                                                                   
04081  9700-EXIT.                                                       
04082      EXIT.                                                        
04083                                                                   
04084  9800-DEEDIT.                                                     
04085                                                                   
04086      EXEC CICS BIF DEEDIT                                         
04087          FIELD   (WS-DEEDIT-FIELD)                                
04088          LENGTH  (WS-DEEDIT-LENGTH)                               
04089      END-EXEC.                                                    
04090                                                                   
04091  9800-EXIT.                                                       
04092      EXIT.                                                        
04093                                                                   
04094  9900-ERROR-FORMAT.                                               
04095      IF NOT EMI-ERRORS-COMPLETE                                   
04096          MOVE LINK-001           TO PGM-NAME                      
04097          EXEC CICS LINK                                           
04098              PROGRAM    (PGM-NAME)                                
04099              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
04100              LENGTH     (EMI-COMM-LENGTH)                         
04101          END-EXEC.                                                
04102                                                                   
04103  9900-EXIT.                                                       
04104      EXIT.                                                        
04105                                                                   
04106  9990-ABEND.                                                      
04107      MOVE -1                     TO  ENTERPFL.                    
04108      MOVE LINK-004               TO  PGM-NAME.                    
04109                                                                   
04110      MOVE DFHEIBLK               TO  EMI-LINE1                    
04111      EXEC CICS LINK                                               
04112          PROGRAM   (PGM-NAME)                                     
04113          COMMAREA  (EMI-LINE1)                                    
04114          LENGTH    (72)                                           
04115      END-EXEC.                                                    
04116                                                                   
04117      MOVE EMI-LINE1              TO  ERRMSG1O.                    
04118      GO TO 8200-SEND-DATAONLY.                                    
04119                                                                   
04120  EJECT                                                            
04121  9995-SECURITY-VIOLATION.                                         
04122                              COPY ELCSCTP.                        
04123                                                                   EL1502
