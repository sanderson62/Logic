      *$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      ****************************************************************
00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID. ECS043SL.                               
00009 *AUTHOR.     Cowtown.
00011                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    THIS PROGRAM WILL READ THE EP-EC FILE AND PRINT      
00025 *            EARNED REVIEW STATEMENTS for Service LIfe.                            
00026                                                                   
060402******************************************************************
060402*                   C H A N G E   L O G
060402*
060402* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060402*-----------------------------------------------------------------
060402*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060402* EFFECTIVE    NUMBER
060402*-----------------------------------------------------------------
060402* 060402    2002060300014  SMVA  MOVE PAGE # FROM BOTTOM TO TOP
061302* 061302    2002061200004  SMVA  REMOVE ECS043B HARDCOPY - PRTN043
090908* 090908    2008090300001  AJRA  FIX RETRO CALC FOR COMMISSION 
090908*                                FIX DIVIDE BY ZERO
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
011315* 011315 CR2014072900001   PEMA  CLONE ECS043 TO MAKE MEP REPORT
030117* 030117 IR2017030100002   PEMA  Correct last year-end-date calc
010722* 010722 CR2019012500003   PEMA  Convert to SQLSERVER 2016
060402******************************************************************
00045                                                                   
00046                                                                   
00047  ENVIRONMENT DIVISION.                                            
00048  INPUT-OUTPUT SECTION.                                            
00049  FILE-CONTROL.                                                    
00050                                                                   
00051      SELECT  SORT-WORK       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  
00052      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   
00053      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   
00054      SELECT  eracct          ASSIGN TO ERACCTT
00055                              ACCESS IS SEQUENTIAL                 
00056                              ORGANIZATION IS INDEXED              
00057                              FILE STATUS IS ERACCT-FILE-STATUS   
00058                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00059      SELECT  ERPLAN-IN       ASSIGN TO ERPLAN
00060                              ORGANIZATION IS INDEXED              
00061                              ACCESS IS DYNAMIC                    
00062                              FILE STATUS IS ERPLAN-FILE-STATUS    
00063                              RECORD KEY IS PL-CONTROL-PRIMARY.    
00064      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
           SELECT  INDEX-FILE      ASSIGN TO SYS011
              organization line sequential.
00065      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.
00067                                                                   
00069  DATA DIVISION.                                                   
00070  FILE SECTION.                                                    

00072  SD  SORT-WORK.                                                   
00073                                                                   
00074  01  SW-REC-OUT.                                                  
           05  sw-retro-group          pic x(6).
00076      05  SW-ACCT-KEY.
               10  SW-CARRIER          PIC X.
               10  SW-GROUP            PIC X(6).
               10  SW-STATE            PIC XX.
               10  SW-ACCOUNT          PIC X(10).
           05  SW-EXP-DTE              PIC 9(8).
           05  sw-acct-profile.
               10  sw-acct-name        pic x(30).
               10  sw-acct-addr1       pic x(30).
               10  sw-acct-city-state  pic x(30).
               10  sw-acct-zip         pic x(9).
               10  sw-acct-rptcd1      pic x(10).
               10  sw-acct-rptcd2      pic x(10).
               10  sw-acct-rptcd3      pic x(10).
           05  sw-accums               PIC X(840).

00085  FD  PRNTR                                                        
           RECORDING MODE F
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  PRT.
           12  P-CTL               PIC  X.
           12  P-DATA              PIC  X(150).


00089  FD  EPEC-FILE                                                    
00090                              COPY ECSEPCFD.                       
00091                              COPY ECSEPC01.                       
00092                                                                   
00094  FD  eracct.                                                    
00095                              COPY ERCACCT.                        
00096                                                                   
00098  FD  ERPLAN-IN.
00099                              COPY ERCPLAN.                        
00100                                                                   
00102  FD  DISK-DATE                                                    
00103                              COPY ELCDTEFD.                       
00104                                                                   
       FD  INDEX-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
       01  INDEX-FILE-REC              PIC X(70).

00106  FD  FICH
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
     
       01  FICH-REC                            PIC   X(151).

00108                                                                   
00110  WORKING-STORAGE SECTION.                                         
00111                                                                   
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

00112  77  FILLER  PIC X(32) VALUE '********************************'.  
00113  77  FILLER  PIC X(32) VALUE '   ECS043SL WORKING STORAGE     '.  
00114  77  FILLER  PIC X(32) VALUE '***********VMOD=2.020 **********'.  
00115                                                                   
       77  ws-work-factor              pic s9(3)v9(8) comp-3 value +0.
       77  ws-retro-rpt-cd-3           pic x(10).
       77  ws-ret-grp-sw               pic x value ' '.
           88  active-group              value 'Y'.
       77  ws-recs-in                  pic 9(5) value zeros.
       77  ws-eof-sw                   pic x  value ' '.
           88  end-of-input              value 'Y'.
       77  ws-eracct-sw                pic x value ' '.
           88  end-of-eracct             value 'Y'.
00116  77  NO-OF-RECORDS-RELEASED  PIC S9(9)      COMP-3  VALUE +0.     
00117  77  NDX                     PIC S999            COMP.            
00118  77  SA                      PIC S999            COMP.            
00119  77  SB                      PIC S999            COMP.            
00120  77  SC                      PIC S999            COMP.            
00121  77  SE                      PIC S999            COMP.            
00122  77  SF                      PIC S999            COMP.            
00123  77  SG                      PIC S999            COMP.            
00124  77  SH                      PIC S999            COMP.            
00125  77  CL                      PIC S999            COMP.            
00126  77  AL                      PIC S999            COMP.            
00127  77  AN                      PIC S999            COMP.            
00128  77  WS-SEQ-NBR              PIC S9              COMP   VALUE +0. 
00129  77  PAGE-CNT                PIC S9(5)           COMP-3 VALUE +0. 
00130  77  LINE-CNT                PIC S999            COMP-3 VALUE +99.
00131  77  PGM-SUB                 PIC S999            COMP-3 VALUE +43.
00132  77  X                       PIC X.                               
00133  77  BREAK-1-SWITCH          PIC X       VALUE SPACES.            
00134  77  WS-EP-CODE              PIC X.                               
00135  77  WS-OB-CODE              PIC X.                               
       77  ws-work-ep              pic s9(9)v99 comp-3 value +0.
00136  77  SAVE-DA-UP              PIC S9(9)V99.                        
00137  77  SAVE-DA-UC              PIC S9(9)V99.                        
00138  77  SAVE-DA-GA-UC           PIC S9(9)V99.                        
00139  77  SAVE-DA-PCT-E-PREM      PIC S9(9)V99.                        
00140  77  SAVE-DA-PCT-UP          PIC S9(9)V99.                        
00141  77  SAVE-BEG-DATE           PIC 9(11)     VALUE ZERO.            
00142  77  WS-RETRO-FLAG           PIC X.                               
       77  i9                      pic s999 value +0 comp-3.
       77  o9                      pic s999 value +0 comp-3.
       77  i0                      pic s999 value +0 comp-3.
       77  i1                      pic s999 value +0 comp-3.
       77  a1                      pic s999 value +0 comp-3.
       77  d1                      pic s999 value +0 comp-3.
       77  d1m                     pic s999 value +0 comp-3.
       77  ws-rel-recs             pic 9(7) value zeros.
       77  ws-rel-drs              pic 9(7) value zeros.
       77  ws-work-loss-ratio      pic s9(5)v9(5) comp-3 value +0.
061814 01  P pointer.
061814 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
061814 01  var-ptr pointer.
061814 01  env-var-len                 pic 9(4)  binary.
061814 01  rc                          pic 9(9)  binary.
061814
061814 01  WS-KIXSYS.
061814     05  WS-KIX-FIL1             PIC X(10).
061814     05  WS-KIX-APPS             PIC X(10).
061814     05  WS-KIX-ENV              PIC X(10).
061814     05  WS-KIX-MYENV            PIC X(10).
061814     05  WS-KIX-SYS              PIC X(10).
061814
CIDMOD 01  SPACE-LINE                PIC X(132)  VALUE SPACES.          
CIDMOD                                                                  


       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

       01  table-data.
           12  tb-retro-group          pic x(10).
           12  tb-retro-status         PIC 9(7).
           12  tb-report-code-3        pic x(16).
           12  tb-retro-name           pic x(50).
           12  tb-retro-eff-dte        pic x(30).
           12  tb-retro-next-rpt-mo    pic 99.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  ws-retro-group-table.
NTTDel*    05  ws-retro-table occurs 300 indexed by r1
NTTDel*      ascending key is ws-retro-group.
NTTIns     05  ws-retro-table occurs 300 times 
NTTIns       ascending key is ws-retro-group indexed by r1.
               10  ws-retro-group      pic x(10).
               10  ws-status           pic 9(5).
               10  ws-report-code-3    pic x(10).
               10  ws-retro-name       pic x(50).
               10  ws-retro-eff-dte    pic x(10).
               10  ws-retro-next-rpt-mo pic 99.


       01  index-record.
           05  ir-page-no              pic 9(5)   value zeros.
           05  ir-pool-code            pic x(6)   value spaces.
           05  ir-rptcd1               pic x(10)  value spaces.
           05  ir-rptcd2               pic x(10)  value spaces.
           05  ir-rptcd3               pic x(10)  value spaces.
           05  ir-carrier              pic x      value spaces.
           05  ir-group                pic x(6)   value spaces.
           05  ir-state                pic xx     value spaces.
           05  ir-account              pic x(10)  value spaces.

00144  01  WS-ABEND-STORAGE.                                            
00145      12  WS-RETURN-CODE        PIC S9(4)   VALUE ZERO COMP.       
00146      12  WS-ABEND-MESSAGE      PIC X(80)   VALUE SPACES.          
00147      12  WS-ABEND-FILE-STATUS  PIC XX      VALUE ZERO.            
00148      12  WS-ZERO               PIC S9      VALUE ZERO COMP-3.     
00149                                                                   
00150      12  WS-ABEND-CODE         PIC 9(4).                          
00151      12  WORK-ABEND-CODE       REDEFINES     WS-ABEND-CODE.       
00152          16  W-ABEND-CODE-1    PIC XX.                            
00153          16  W-ABEND-CODE-2    PIC XX.                            
00154      12  ERACCT-FILE-STATUS   PIC XX.                            
00155      12  ERPLAN-FILE-STATUS    PIC XX.                            
00156                                                                   
00157  01  WORK-MSG.                                                    
00158      05  FILLER              PIC X    VALUE SPACE.                
00159      05  W-MM                PIC XX.                              
00160      05  FILLER              PIC X    VALUE '/'.                  
00161      05  W-DD                PIC XX.                              
00162      05  FILLER              PIC X    VALUE '/'.                  
00163      05  W-CCYY              PIC X(4).                            
00164      05  FILLER        PIC X(9)  VALUE ' TO DATE '.               

00222  01  WS-ACCT-CONTROL.                                             
00223      12  WS-MSTR-CNTRL       PIC X(25)   VALUE LOW-VALUES.        
00224      12  WS-EFFECT           PIC 9(11)  COMP-3  VALUE ZEROS.      

00226  01  WS-SAVE-CONTROL.                                             
00227      12  WS-SAVE-CARR        PIC X.                               
00228      12  WS-SAVE-COMP        PIC XXXXXX.                          
00229      12  WS-SAVE-STATE       PIC XX.                              
00230      12  WS-SAVE-ACCT        PIC X(10).                           
00231      12  WS-SAVE-EXP-DATE    PIC 9(11)  COMP-3.                   
00232      12  WS-SAVE-EFF-DATE    PIC 9(11)  COMP-3.                   

00234  01  WS-SAVE-CTL.                                                 
00235      12  WS-SAVE-GA          PIC X(10).                           
00236      12  WS-SAVE-CAR         PIC X.                               
00237      12  WS-SAVE-CMP         PIC XXXXXX.                          
00238      12  WS-SAVE-ST          PIC XX.                              
00239      12  WS-SAVE-ACC         PIC X(10).                           
00240      12  WS-SAVE-EXP         PIC 9(11)  COMP-3.                   

00242  01  WS-PLAN-CONTROL.                                             
00243      12  WS-PL-COMPANY-CD    PIC X          VALUE SPACE.          
00244      12  WS-PL-CONTROL-A.                                         
00245          16  WS-PL-CARRIER       PIC X      VALUE SPACE.          
00246          16  WS-PL-GROUPING      PIC X(6)   VALUE SPACES.         
00247          16  WS-PL-STATE         PIC XX     VALUE SPACES.         
00248          16  WS-PL-ACCOUNT       PIC X(10)  VALUE SPACES.         
00249      12  WS-PL-BENEFIT-TYPE  PIC X          VALUE SPACE.          
00250      12  WS-PL-BENEFIT-CODE  PIC XX         VALUE SPACE.          
00251      12  WS-PL-REVISION-NO   PIC X(3)       VALUE SPACES.         

00254  01  MISC.                                                        
           12  ws-curr-date            pic 9(8).
           12  ws-last-month-end       pic 9(8).
           12  ws-last-month-end-x redefines ws-last-month-end.
               16  ws-lme-ccyy         pic 9(4).
               16  ws-lme-mm           pic 99.
               16  ws-lme-dd           pic 99.
           12  ws-last-year-end        pic 9(8).
           12  ws-last-year-end-x redefines ws-last-year-end.
               16  ws-lye-ccyy         pic 9(4).
               16  ws-lye-mm           pic 99.
               16  ws-lye-dd           pic 99.
00255      12  RETENT-FACT         PIC S9V9(5)   VALUE +0.              
00256      12  ERP-PERCENT         PIC S9V9(5)   VALUE +0.              
00257      12  EXP-FACTOR          PIC S9V9(5)   VALUE +0.              
00258      12  LIFE-RETENT         PIC S9V9(5)     COMP-3  VALUE +0.    
00259      12  AH-RETENT           PIC S9V9(5)     COMP-3  VALUE +0.    
00260      12  SAVE-RETRO-LIMIT    PIC S9(7)       COMP-3.              
00261      12  ANNUALIZED-PREM     PIC S9(9)V99    COMP-3.              
00262      12  WS-MIN-LOSS         PIC S9(5)V99    COMP-3.              
00263      12  HEAD-SW             PIC X         VALUE SPACE.           
00264      12  NBR-OF-MTHS         PIC 99        VALUE 12.              
00265      12  SAVE-GA-FLAG        PIC X.                               
00266      12  SAVE-RET-Y-N        PIC X.                               
00267      12  SAVE-RET-TYP        PIC X.                               
00268      12  SAVE-PREM-P-E       PIC X.                               
00269      12  SAVE-CLMS-P-I       PIC X.                               
00270      12  WS-EXCLUSION-SWITCH PIC X             VALUE SPACE.       
00271          88  EXCLUDING-BENEFIT  VALUE '*'.                        
00272                                                                   
00273  01  BEG-DATE                PIC 9(11).                           
00274  01  BEG-DATE-R REDEFINES BEG-DATE.                               
00275      12 FILLER               PIC 999.                             
00276      12  BD-CCYY             PIC 9(04).                           
00277      12  BD-CCYR REDEFINES BD-CCYY.                               
00278          16  BD-CC           PIC 99.                              
00279          16  BD-YR           PIC 99.                              
00280      12  BD-MO               PIC 99.                              
00281      12  BD-DA               PIC 99.                              
00282                                                                   
00283  01  RUN-DT                  PIC 9(11).                           
00284  01  RUN-DT-R REDEFINES RUN-DT.                                   
00285      12  FILLER              PIC 999.                             
00286      12  RD-CCYY             PIC 9(04).                           
00287      12  RD-CCYR REDEFINES RD-CCYY.                               
00288          16  RD-CC           PIC 99.                              
00289          16  RD-YR           PIC 99.                              
00290      12  RD-MO               PIC 99.                              
00291      12  RD-DA               PIC 99.                              
00292                                                                   
00293  01  WORK-DATE           PIC 9(11).                               
00294  01  WORK-DATE-R  REDEFINES  WORK-DATE.                           
00295      12  FILLER          PIC 999.                                 
00296      12  WK-CCYY         PIC 9(04).                               
00297      12  WK-CCYR REDEFINES WK-CCYY.                               
00298          16  WK-CC       PIC 99.                                  
00299          16  WK-YR       PIC 99.                                  
00300      12  WK-MO           PIC 99.                                  
00301      12  WK-DA           PIC 99.                                  
00302                                                                   
00303  01  ZONE-GROUP.                                                  
00304      03  ZON                 PIC XXX       VALUE 'ZON'.           
00305      03  Z-G                 PIC XXX.                             

      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   Occurrence 1   detail (account date range)               ***
      ***              2   account level totals                      ***
      ***              3   Retro Group level totals                  ***
      ***              4   Final report totals                       ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

       01  ws-accum-work-area.
          02  ws-accum-record occurs 4.
           05  ws-cur-accums occurs 2.
               10  cur-gross-prem      pic s9(11)v99 comp-3.
               10  cur-cancel-prem     pic s9(11)v99 comp-3.
               10  cur-net-prem        pic s9(11)v99 comp-3.
               10  cur-uep             pic s9(11)v99 comp-3.
               10  cur-earn-prem       pic s9(11)v99 comp-3.
               10  cur-inv-income      pic s9(11)v99 comp-3.
               10  cur-tot-income      pic s9(11)v99 comp-3.
               10  cur-acct-comms      pic s9(11)v99 comp-3.
               10  cur-ow-comms        pic s9(11)v99 comp-3.
               10  cur-ret-fees        pic s9(11)v99 comp-3.
               10  cur-clm-pmts        pic s9(11)v99 comp-3.
               10  cur-prem-tax        pic s9(11)v99 comp-3.
               10  cur-other-expenses  pic s9(11)v99 comp-3.
               10  cur-tot-expenses    pic s9(11)v99 comp-3.
               10  cur-net-cash-flow   pic s9(11)v99 comp-3.
               10  cur-mort-resv       pic s9(11)v99 comp-3.
               10  cur-clm-resv        pic s9(11)v99 comp-3.
               10  cur-net-mep-comm    pic s9(11)v99 comp-3.
               10  cur-retro-pmts      pic s9(11)v99 comp-3.
               10  cur-mep-due         pic s9(11)v99 comp-3.

           05  ws-lme-accums occurs 2.
               10  lme-gross-prem      pic s9(11)v99 comp-3.
               10  lme-cancel-prem     pic s9(11)v99 comp-3.
               10  lme-net-prem        pic s9(11)v99 comp-3.
               10  lme-uep             pic s9(11)v99 comp-3.
               10  lme-earn-prem       pic s9(11)v99 comp-3.
               10  lme-inv-income      pic s9(11)v99 comp-3.
               10  lme-tot-income      pic s9(11)v99 comp-3.
               10  lme-acct-comms      pic s9(11)v99 comp-3.
               10  lme-ow-comms        pic s9(11)v99 comp-3.
               10  lme-ret-fees        pic s9(11)v99 comp-3.
               10  lme-clm-pmts        pic s9(11)v99 comp-3.
               10  lme-prem-tax        pic s9(11)v99 comp-3.
               10  lme-other-expenses  pic s9(11)v99 comp-3.
               10  lme-tot-expenses    pic s9(11)v99 comp-3.
               10  lme-net-cash-flow   pic s9(11)v99 comp-3.
               10  lme-mort-resv       pic s9(11)v99 comp-3.
               10  lme-clm-resv        pic s9(11)v99 comp-3.
               10  lme-net-mep-comm    pic s9(11)v99 comp-3.
               10  lme-retro-pmts      pic s9(11)v99 comp-3.
               10  lme-mep-due         pic s9(11)v99 comp-3.

           05  ws-lye-accums occurs 2.
               10  lye-gross-prem      pic s9(11)v99 comp-3.
               10  lye-cancel-prem     pic s9(11)v99 comp-3.
               10  lye-net-prem        pic s9(11)v99 comp-3.
               10  lye-uep             pic s9(11)v99 comp-3.
               10  lye-earn-prem       pic s9(11)v99 comp-3.
               10  lye-inv-income      pic s9(11)v99 comp-3.
               10  lye-tot-income      pic s9(11)v99 comp-3.
               10  lye-acct-comms      pic s9(11)v99 comp-3.
               10  lye-ow-comms        pic s9(11)v99 comp-3.
               10  lye-ret-fees        pic s9(11)v99 comp-3.
               10  lye-clm-pmts        pic s9(11)v99 comp-3.
               10  lye-prem-tax        pic s9(11)v99 comp-3.
               10  lye-other-expenses  pic s9(11)v99 comp-3.
               10  lye-tot-expenses    pic s9(11)v99 comp-3.
               10  lye-net-cash-flow   pic s9(11)v99 comp-3.
               10  lye-mort-resv       pic s9(11)v99 comp-3.
               10  lye-clm-resv        pic s9(11)v99 comp-3.
               10  lye-net-mep-comm    pic s9(11)v99 comp-3.
               10  lye-retro-pmts      pic s9(11)v99 comp-3.
               10  lye-mep-due         pic s9(11)v99 comp-3.

       01  filler.
           05  ws-acct-table.
               10  ws-hold-dr-records occurs 30
                                       pic x(972).

       01  filler.
           05  ws-gross            pic s9(11)v99 comp-3.
           05  ws-cancel           pic s9(11)v99 comp-3.
           05  ws-uep              pic s9(11)v99 comp-3.
           05  ws-clm-pmts         pic s9(11)v99 comp-3.
           05  ws-acct-comms       pic s9(11)v99 comp-3.
           05  ws-ow-comms         pic s9(11)v99 comp-3.
           05  ws-tot-comms        pic s9(11)v99 comp-3.

01011  01  COMPARE-DATE-TABLE.                                          
01012 *    05  COMPARE-DTS OCCURS 13 TIMES.                             
122804     05  COMPARE-DTS OCCURS 16.
01013          10  COMPARE-DT.                                          
01014              15  COMP-CCYY       PIC 9(04).                       
01015              15  COMP-CCYR REDEFINES COMP-CCYY.                   
01016                  20  COMP-CC     PIC 99.                          
01017                  20  COMP-YR     PIC 99.                          
01018              15  COMP-MO         PIC 99.                          
01019                                                                   
01020  01  COMPARE-DATE9TABLE REDEFINES COMPARE-DATE-TABLE.             
01021 *    05  COMPARE9DTS OCCURS 13 TIMES.                             
122804     05  COMPARE9DTS OCCURS 16.
01022          10  COMPARE9DT          PIC 9(06).                       
01023                                                                   
122804 01  TWO-YEARS-AGO               PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEAR-ENDS-AGO           PIC 9(06)  VALUE ZEROS.
122804 01  YEAR-END-DT                 PIC 9(06)  VALUE ZEROS.

       01  ws-prev-epec-key.
           05  ws-prev-full-key.
               10  ws-prev-retro-group pic x(6).
               10  ws-prev-acct-dr-key.
                   15  ws-prev-acct-key.
                       20  ws-prev-carrier  pic x.
                       20  ws-prev-grouping pic x(6).
                       20  ws-prev-state    pic xx.
                       20  ws-prev-account  pic x(10).
                   15  ws-prev-exp-dte      pic 9(8).

       01  ws-comp-epec-key.
           05  ws-comp-full-key.
               10  ws-comp-retro-group pic x(6).
               10  ws-comp-acct-dr-key.
                   15  ws-comp-acct-key.
                       20  ws-comp-carrier  pic x.
                       20  ws-comp-grouping pic x(6).
                       20  ws-comp-state    pic xx.
                       20  ws-comp-account  pic x(10).
                   15  ws-comp-exp-dte      pic 9(8).

       01  filler.
           05  ws-acct-name            pic x(30).
           05  ws-acct-addr1           pic x(30).
           05  ws-acct-addr2           pic x(30).
           05  ws-acct-city-state      pic x(30).
           05  ws-acct-zip             pic x(9).
           05  ws-acct-ret-grp         pic x(6).
           05  ws-acct-rptcd1          pic x(10).
           05  ws-acct-rptcd2          pic x(10).
           05  ws-acct-rptcd3          pic x(10).

00487  01  HEAD-1.
00488      12  H1-NAME         PIC X(30)   VALUE SPACES. 
00489      12  FILLER          PIC X(30)   VALUE SPACES.                
00490      12  FILLER          PIC X(30)   VALUE 'CREDIT INSURANCE EARNE
      -        'D REVIEW'.
00492      12  FILLER          PIC X(46)   VALUE SPACES.                
00493      12  FILLER          PIC X(8)    VALUE 'ECS043SL'.            
00495                                                                   
00496  01  HEAD-2.                                                      
00497      12  H2-ADDR         PIC X(30)   VALUE SPACES.                
00498      12  FILLER          PIC X(30)   VALUE SPACES.                
00499      12  HD-COMP         PIC X(30)   VALUE SPACES.                
00500      12  FILLER          PIC X(46)   VALUE SPACES.                
00501      12  HD-RD           PIC X(8)    VALUE SPACES.                
00502                                                                   
00503  01  HEAD-3.                                                      
00504      12  H3-ADDR         PIC X(40)   VALUE SPACES.
00505 *    12  FILLER          PIC X       VALUE SPACES.                
00506 *    12  H3-ZIP          PIC X(9)    VALUE SPACES.                
00520      12  FILLER          PIC X(24)   VALUE SPACES.                
00521      12  HD-AD           PIC X(18)   VALUE SPACES.                
060402     12  FILLER          PIC X(54)   VALUE SPACES.
060402     12  FILLER          PIC X(05)   VALUE
060402         'PAGE '.
060402     12  H3-PAGE         PIC ZZZZ9.

00524  01  HEAD-4.                                                      
00525      12  H4-STACCT       PIC X(12)   VALUE 'ST ACCOUNT  '.
           12  filler          pic x(95)   value spaces.
           12  filler          pic x(17)   value 'Effective Date:  '.
           12  h4-eff-dte      pic x(10)   value spaces.
00529                                                                   
00530  01  HEAD-5.                                                      
00534      12  H5-ST           PIC XX      VALUE SPACES.                
00535      12  FILLER          PIC X       VALUE SPACES.                
00536      12  H5-ACCT         PIC X(10)   VALUE SPACES.                
00537      12  FILLER          PIC X       VALUE SPACES.                
00538      12  H5-FMO          PIC XX      VALUE SPACES.                
00539      12  H5-FS1          PIC X       VALUE '-'.
00540      12  H5-FDA          PIC XX      VALUE SPACES.                
00541      12  H5-FS2          PIC X       VALUE ' '.                   
           12  h5-group        pic x(10)   value spaces.
00542      12  H5-FYR          PIC XX      VALUE SPACES.                
           12  h5-name         pic x(50)   value spaces.
           12  filler          pic x(21)   value spaces.
           12  filler          pic x(21)   value
              '  Next Anniversary:  '.
           12  h5-anniv-dte    pic x(5)    value spaces.
00543 *    12  FILLER          PIC X       VALUE SPACES.                
00544 *    12  H5-TMO          PIC XX      VALUE SPACES.                
00545 *    12  H5-TS1          PIC X       VALUE ' '.                   
00546 *    12  H5-TDA          PIC XX      VALUE SPACES.                
00547 *    12  H5-TS2          PIC X       VALUE ' '.                   
00548 *    12  H5-TYR          PIC XX      VALUE SPACES.                
00549 *    12  FILLER          PIC X(4)    VALUE SPACES.                
00550 *    12  H5-GROUP        PIC X(50)   VALUE SPACES.                
00551 *    12  H5-MESSAGE      PIC X       VALUE SPACES.                
CIDMOD*    12  END-CHARACTER   PIC X       VALUE ' '.                   

       01  head-6.
           05  f                       pic x(49) value spaces.
           05  f                       pic x(13) value 'Current Month'.
           05  F                       pic x(26) value spaces.
           05  f                       pic x(12) value 'Year To Date'.
           05  F                       pic x(24) value spaces.
           05  f                    pic x(17) value 'Inception To Date'.

       01  head-7.
           05  f                       pic x(146) value '               
      -    '                                Life                A&H     
      -    '          Life                A&H               Life        
      -    '        A&H'.

       01  head-8.
           05  f                       pic x(16) value
               ' R e v e n u e s'.

       01  head-9.
           05  f                       pic x(16) value
               ' E x p e n s e s'.

       01  head-10.
           05  f                       pic x(24) value
               ' N e t  C a s h  F l o w'.

       01  DETAIL-DESC-LINE.
           05  det-misc-desc   pic x(30).
       01  DETAIL-LINE.
           05  det-desc        pic x(25).
           05  f               pic x(04).
           05  det-mtd-lf      pic zzz,zzz,zz9-.
           05  f               pic x(5).
           05  det-mtd-ah      pic zzz,zzz,zz9-.
           05  f               pic x(7).
           05  det-ytd-lf      pic zzz,zzz,zz9-.
           05  f               pic x(7).
           05  det-ytd-ah      pic zzz,zzz,zz9-.
           05  f               pic x(6).
           05  det-itd-lf      pic zzz,zzz,zz9-.
           05  f               pic x(6).
           05  det-itd-ah      pic zzz,zzz,zz9-.
           05  f               pic x(6).
           05  det-itd-tot     pic zzz,zzz,zz9-.

       01  MEP-DETAIL-LINE.
           05  mep-desc        pic x(25).
           05  f               pic x(04).
           05  mep-mtd-lf      pic zzz,zzz,zz9- blank when zero.
           05  f               pic x(5).
           05  mep-mtd-ah      pic zzz,zzz,zz9- blank when zero.
           05  f               pic x(7).
           05  mep-ytd-lf      pic zzz,zzz,zz9- blank when zero.
           05  f               pic x(7).
           05  mep-ytd-ah      pic zzz,zzz,zz9- blank when zero.
           05  f               pic x(6).
           05  mep-itd-lf      pic zzz,zzz,ZZ9- blank when zero.
           05  f               pic x(6).
           05  mep-itd-ah      pic zzz,zzz,ZZ9- blank when zero.
           05  f               pic x(6).
           05  mep-itd-tot     pic zzz,zzz,ZZ9-.

       01  ilr-DETAIL-LINE.
           05  ilr-desc        pic x(25).
           05  f               pic x(05).
           05  ilr-mtd-lf      pic zzz,zz9.99-.
           05  f               pic x(6).
           05  ilr-mtd-ah      pic zzz,zz9.99-.
           05  f               pic x(8).
           05  ilr-ytd-lf      pic zzz,zz9.99-.
           05  f               pic x(8).
           05  ilr-ytd-ah      pic zzz,zz9.99-.
           05  f               pic x(7).
           05  ilr-itd-lf      pic zzz,ZZ9.99-.
           05  f               pic x(7).
           05  ilr-itd-ah      pic zzz,ZZ9.99-.
           05  f               pic x(7).
           05  ilr-itd-tot     pic zzz,ZZ9.99-.

00630                                  COPY ELCDATE.                 
00632                                  COPY ELCDTECX.                
00634                                  COPY ELCDTEVR.                
00636                                  COPY ELCACCTV.                
00638                                  COPY ELCEPCVR.                

       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.
       01  var  pic x(30).

00641  PROCEDURE DIVISION.                                              
00643                                  COPY ELCDTERX.

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0                   to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if
      
           display ' KIXSYS  ' ws-kix-myenv

           perform 1000-connect        thru 1000-exit
           perform 1010-declare-cursor thru 1010-exit
           perform 1020-open-cursor    thru 1020-exit
           perform 1030-process-input  thru 1030-exit
           perform 1050-disconnect     thru 1050-exit
           perform varying i1 from +1 by +1
              until (i1 > +300)
              or (ws-retro-group (i1) = high-values)
              display ' group status ' ws-retro-group (i1) ' '
                 ws-status (i1)
           end-perform

           perform 0010-open-files     thru 0010-exit

           sort sort-work on ascending sw-retro-group sw-acct-key
              input procedure
                 0000-sort-input       thru 0000-exit
              output procedure
                 4000-sort-output      thru 4000-exit

           perform 5000-close-files    thru 5000-exit
           goback

           .
       0000-sort-input.

           perform 0020-init           thru 0020-exit
           perform 0100-process-input  thru 0100-exit until
              end-of-input

           perform 3300-release-rec    thru 3300-exit

           display ' released recs ' ws-rel-recs
           display ' released drs  ' ws-rel-drs

           .
       0000-exit.
           exit.

       0010-open-files.

00778      OPEN INPUT                                                   
00779              eracct                                             
00780              ERPLAN-IN
00781              EPEC-FILE                                            
00782          OUTPUT
                   index-file
00783              PRNTR
00784                                                                   
00785      IF ERACCT-FILE-STATUS  = '00' OR '97'                       
00786          NEXT SENTENCE                                            
00787        ELSE                                                       
00788          MOVE '11'                TO W-ABEND-CODE-1               
00789          MOVE ERACCT-FILE-STATUS TO W-ABEND-CODE-2               
00790          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               
00791          MOVE 'OPEN ERROR - ERACCTT' TO WS-ABEND-MESSAGE          
00792          GO TO ABEND-PGM.                                         
00793                                                                   
00794      IF ERPLAN-FILE-STATUS  = '00' OR '97'                        
00795          NEXT SENTENCE                                            
00796        ELSE                                                       
00797          MOVE '12'                TO W-ABEND-CODE-1               
00798          MOVE ERPLAN-FILE-STATUS  TO W-ABEND-CODE-2               
00799          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               
00800          MOVE 'OPEN ERROR - ERPLAN ' TO WS-ABEND-MESSAGE          
00801          GO TO ABEND-PGM.                                         
00802                                                                   
           .
       0010-exit.
           exit.

       0020-init.

           move +1                     to i0

           MOVE WS-CURRENT-DATE        TO HD-RD
           MOVE ALPH-DATE              TO HD-AD
           MOVE COMPANY-NAME           TO HD-COMP

           MOVE RUN-DATE               TO RUN-DT
                                          BEG-DATE

           move run-date               to ws-curr-date

           move bin-run-date           to dc-bin-date-1
           move '6'                    to dc-option-code
           move -1                     to dc-elapsed-months
           move +0                     to dc-elapsed-days
           move '1'                    to dc-end-of-month
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              display ' last month end ' dc-greg-date-cymd
              move dc-greg-date-cymd   to ws-last-month-end
           else
              display ' last month date error ' dc-error-code
              perform abend-pgm
           end-if

030117*    move ws-last-month-end      to ws-last-year-end
030117     move ws-curr-date           to ws-last-year-end
           subtract 1 from ws-lye-ccyy
           move 12                     to ws-lye-mm
           move 31                     to ws-lye-dd

           display ' current month end ' ws-curr-date
           display ' last month end    ' ws-last-month-end
           display ' last year end     ' ws-last-year-end

           perform varying i0 from +1 by +1 until i0 > +4
              perform 0050-init-acct-accums
                                       thru 0050-exit
           end-perform

           perform 0030-read-input     thru 0030-exit

           move ep-carrier             to ws-prev-carrier
           move ep-grouping            to ws-prev-grouping
           move ep-state               to ws-prev-state
           move ep-account             to ws-prev-account
           move ep-exp-dte             to ws-prev-exp-dte

           move low-values             to am-control-primary
           move dte-clasic-company-cd  to am-company-cd
           start eracct key >= am-control-primary
           perform 0120-eracct-read-next
                                       thru 0120-exit

           .
       0020-exit.
           exit.

01002  0030-read-input.

01003      READ EPEC-FILE AT END                                        
01004         MOVE HIGH-VALUES         TO EP-CONTROL
              set end-of-input to true
           end-read

pemtst*    if ep-carrier = '9'
pemtst*       set end-of-input to true
pemtst*    end-if

           if end-of-input
              go to 0030-exit
           end-if
01006                                                                   
01007      IF (EP-RECORD-ID NOT = 'EP' AND 'EC')
                      or
              (ep-rein = 'R')
                      or
              (ep-run-dte > run-dt)
01008         GO TO 0030-read-input
           end-if

           move ep-cntrl-1             to ws-comp-acct-key
           move ep-exp-dte             to ws-comp-exp-dte

           .
01018  0030-EXIT.                                                       
01019      EXIT.                                                        

       0050-init-acct-accums.

           perform varying i1 from +1 by +1 until i1 > +2
              move +0                  to cur-gross-prem     (i0 i1)
              move +0                  to cur-cancel-prem    (i0 i1)
              move +0                  to cur-net-prem       (i0 i1)
              move +0                  to cur-uep            (i0 i1)
              move +0                  to cur-earn-prem      (i0 i1)
              move +0                  to cur-inv-income     (i0 i1)
              move +0                  to cur-tot-income     (i0 i1)
              move +0                  to cur-acct-comms     (i0 i1)
              move +0                  to cur-ow-comms       (i0 i1)
              move +0                  to cur-ret-fees       (i0 i1)
              move +0                  to cur-clm-pmts       (i0 i1)
              move +0                  to cur-prem-tax       (i0 i1)
              move +0                  to cur-other-expenses (i0 i1)
              move +0                  to cur-tot-expenses   (i0 i1)
              move +0                  to cur-net-cash-flow  (i0 i1)
              move +0                  to cur-mort-resv      (i0 i1)
              move +0                  to cur-clm-resv       (i0 i1)
              move +0                  to cur-net-mep-comm   (i0 i1)
              move +0                  to cur-retro-pmts     (i0 i1)
              move +0                  to cur-mep-due        (i0 i1)
                                       
              move +0                  to lme-gross-prem     (i0 i1)
              move +0                  to lme-cancel-prem    (i0 i1)
              move +0                  to lme-net-prem       (i0 i1)
              move +0                  to lme-uep            (i0 i1)
              move +0                  to lme-earn-prem      (i0 i1)
              move +0                  to lme-inv-income     (i0 i1)
              move +0                  to lme-tot-income     (i0 i1)
              move +0                  to lme-acct-comms     (i0 i1)
              move +0                  to lme-ow-comms       (i0 i1)
              move +0                  to lme-ret-fees       (i0 i1)
              move +0                  to lme-clm-pmts       (i0 i1)
              move +0                  to lme-prem-tax       (i0 i1)
              move +0                  to lme-other-expenses (i0 i1)
              move +0                  to lme-tot-expenses   (i0 i1)
              move +0                  to lme-net-cash-flow  (i0 i1)
              move +0                  to lme-mort-resv      (i0 i1)
              move +0                  to lme-clm-resv       (i0 i1)
              move +0                  to lme-net-mep-comm   (i0 i1)
              move +0                  to lme-retro-pmts     (i0 i1)
              move +0                  to lme-mep-due        (i0 i1)
                                       
              move +0                  to lye-gross-prem     (i0 i1)
              move +0                  to lye-cancel-prem    (i0 i1)
              move +0                  to lye-net-prem       (i0 i1)
              move +0                  to lye-uep            (i0 i1)
              move +0                  to lye-earn-prem      (i0 i1)
              move +0                  to lye-inv-income     (i0 i1)
              move +0                  to lye-tot-income     (i0 i1)
              move +0                  to lye-acct-comms     (i0 i1)
              move +0                  to lye-ow-comms       (i0 i1)
              move +0                  to lye-ret-fees       (i0 i1)
              move +0                  to lye-clm-pmts       (i0 i1)
              move +0                  to lye-prem-tax       (i0 i1)
              move +0                  to lye-other-expenses (i0 i1)
              move +0                  to lye-tot-expenses   (i0 i1)
              move +0                  to lye-net-cash-flow  (i0 i1)
              move +0                  to lye-mort-resv      (i0 i1)
              move +0                  to lye-clm-resv       (i0 i1)
              move +0                  to lye-net-mep-comm   (i0 i1)
              move +0                  to lye-retro-pmts     (i0 i1)
              move +0                  to lye-mep-due        (i0 i1)
           end-perform

           .
       0050-exit.
           exit.

       0100-process-input.

      *    if ws-comp-acct-key not = ws-prev-acct-key
      *       perform 3500-release-drs thru 3500-exit
      *       move spaces              to ws-acct-table
      *       move +0                  to d1 d1m
      *    end-if

           if ws-comp-acct-dr-key not = ws-prev-acct-dr-key
              perform 3300-release-rec thru 3300-exit
           end-if

           perform 0110-match-to-acct  thru 0110-exit

pemtst     if (am-ret-y-n = 'Y')
pemtst        and (ws-retro-rpt-cd-3 = 'SVCLIFE')
pemtst        and (not active-group)
pemtst        display ' bypassing ' am-state ' ' am-account
pemtst        ' ' am-ret-grp
pemtst     end-if

           if (am-ret-y-n = 'Y')
              and (active-group)
              and (ws-retro-rpt-cd-3 = 'SVCLIFE')
              continue
           else
              go to 0100-continue
           end-if

           perform 0300-sync-benefit   thru 0300-exit

           if ep-record-id = 'EP'
              perform 0200-check-for-retro-pmts
                                       thru 0200-exit
           end-if

           if (ep-run-dte = ws-curr-date)
                     or
              (ep-purge = 'P')
              perform 0999-accum-current
                                       thru 0999-exit
           end-if
           if (ep-run-dte = ws-last-month-end)
                    or
              ((ep-purge = 'P')
              and (ep-run-dte < ws-last-month-end))
              perform 2000-accum-last-month-end
                                       thru 2000-exit
           end-if

           if (ep-run-dte = ws-last-year-end)
                    or
              ((ep-purge = 'P')
              and (ep-run-dte < ws-last-year-end))
              perform 3000-accum-last-year-end
                                       thru 3000-exit
           end-if

           .
       0100-continue.

           perform 0030-read-input     thru 0030-exit

           .
       0100-exit.
           exit.

       0110-match-to-acct.

           move spaces                 to ws-ret-grp-sw
                                          ws-retro-rpt-cd-3

           evaluate true
              when ep-cntrl-1 > am-control-a
                 perform 0120-eracct-read-next
                                       thru 0120-exit
                 go to 0110-match-to-acct
              when ep-cntrl-1 < am-control-a
                 display ' no matching acct for EPEC '
                    ep-cntrl-1 ' ' ep-exp-dte
                 perform abend-pgm
              when ep-exp-dte > am-expire-dt
                 perform 0120-eracct-read-next
                                       thru 0120-exit
                 go to 0110-match-to-acct
              when ep-exp-dte < am-expire-dt
                 display ' no matching acct expdt for EPEC '
                    ep-cntrl-1 ' ' ep-exp-dte
                 perform abend-pgm
           end-evaluate

           if am-ret-grp = low-values
              move spaces              to am-ret-grp
           end-if

           move am-report-code-1       to ws-acct-rptcd1
           move am-report-code-2       to ws-acct-rptcd2
           move am-name                to ws-acct-name
           move am-person              to ws-acct-addr1
           move am-addrs               to ws-acct-addr2

           move spaces                 to ws-acct-city-state
           string
              am-addr-city ' '
              am-addr-state
              delimited by '  '        into ws-acct-city-state
           end-string

           move am-zip                 to ws-acct-zip
           move am-ret-grp             to ws-acct-ret-grp

      *    if am-ret-grp = 'MISAUT'
      *       set active-group to true
      *       move 'SVCLIFE'    to ws-retro-rpt-cd-3
      *    else
           search all ws-retro-table at end
              continue
             when am-ret-grp = ws-retro-group (r1)
              set active-group to true
              move ws-report-code-3 (r1) to ws-retro-rpt-cd-3
           end-search
      *    end-if
           move ws-retro-rpt-cd-3      to ws-acct-rptcd3

           .
       0110-exit.
           exit.

       0120-eracct-read-next.

           if end-of-eracct
              display ' somehow got to 0120 after EOF '
              perform abend-pgm
           end-if

           read eracct next record
           if (eracct-file-status = '10' or '23')
              or (am-company-cd not = dte-clasic-company-cd)
              set end-of-eracct to true
           else
              if eracct-file-status not = '00'
                 display ' error-eracct-readnext ' eracct-file-status
                 perform abend-pgm
              end-if
           end-if

           .
       0120-exit.
           exit.


       0200-check-for-retro-pmts.

           if ep-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           compute cur-retro-pmts (i0 i1) =
              cur-retro-pmts (i0 i1) + ep-retro-payments

           if ep-run-dte <= ws-last-month-end
              compute lme-retro-pmts (i0 i1) =
                 lme-retro-pmts (i0 i1) + ep-retro-payments
           end-if

           if ep-run-dte <= ws-last-year-end
              compute lye-retro-pmts (i0 i1) =
                 lye-retro-pmts (i0 i1) + ep-retro-payments
           end-if

           .
       0200-exit.
           exit.

       0300-sync-benefit.

           perform 0310-find-state     thru 0310-exit

           if ep-rcd-type = 'L' OR 'P'
              perform 0320-find-lf-ben thru 0320-exit
           else
              perform 0330-find-ah-ben thru 0330-exit
           end-if

           if state-abbr (clas-indexs) = 'WY'
              move 'P'                 to ws-ep-code
           end-if

           .
       0300-exit.
           exit.

       0310-find-state.

           if ep-state = state-sub (clas-indexs)
              continue
           else
              perform varying clas-indexs from clas-starts by +1
                 until (clas-indexs > clas-maxs)
                 or (ep-state = state-sub (clas-indexs))
              end-perform
           end-if

           if clas-indexs > clas-maxs
              display ' invalid state ' ep-state ' ' ep-account
              perform abend-pgm
           end-if

           .
       0310-exit.
           exit.

       0320-find-lf-ben.

           if ep-ben-code = clas-i-ben (clas-indexl)
              continue
           else
              perform varying clas-indexl from clas-startl by +1
                 until (clas-indexl > clas-maxl)
                 or (ep-ben-code = clas-i-ben (clas-indexl))
              end-perform
           end-if

           if clas-indexl > clas-maxl
              display ' invalid lf benefit code ' ep-ben-code ' '
                 ep-account
              perform abend-pgm
           end-if

           move clas-i-ep (clas-indexl) to ws-ep-code

           .
       0320-exit.
           exit.

       0330-find-ah-ben.

           if ep-ben-code = clas-i-ben (clas-indexa)
              continue
           else
              perform varying clas-indexa from clas-starta by +1
                 until (clas-indexa > clas-maxa)
                 or (ep-ben-code = clas-i-ben (clas-indexa))
              end-perform
           end-if

           if clas-indexa > clas-maxa
              display ' invalid AH benefit code ' ep-ben-code ' '
                 ep-account
              perform abend-pgm
           end-if

           move clas-i-ep (clas-indexa) to ws-ep-code

           .
       0330-exit.
           exit.

       0999-accum-current.

           if ep-record-id = 'EP'
              perform 1100-process-premium
                                       thru 1100-exit
           else
              perform 1200-process-commission
                                       thru 1200-exit
           end-if

           .
       0999-exit.
           exit.

       1100-process-premium.

           if ep-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           compute cur-gross-prem (i0 i1) =
              cur-gross-prem (i0 i1) + ep-iss-prm
           compute cur-cancel-prem (i0 i1) =
              cur-cancel-prem (i0 i1) + ep-cnc-prm

           evaluate true
              when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                 move ep-prm-78     to ws-work-ep
              when ws-ep-code = 'B' or 'K' or 'L'
                 move ep-prm-st     to ws-work-ep
              when ws-ep-code = 'P'
                 move ep-prm-pr     to ws-work-ep
              when ws-ep-code = 'M'
                 compute ws-work-ep rounded =
                    (ep-prm-78 + ep-prm-pr) * .50
              when ws-ep-code = '1'
                 compute ws-work-ep rounded =
                    (ep-prm-78 * .3333) + (ep-prm-pr * .6666)
              when ws-ep-code = '2'
                 compute ws-work-ep rounded =
                    (ep-prm-pr * .80) + (ep-prm-78 * .20)
           end-evaluate

           compute cur-uep (i0 i1) = cur-uep (i0 i1) +
           (ep-iss-prm - ep-cnc-prm - ws-work-ep)
           compute cur-earn-prem (i0 i1) =
           cur-earn-prem (i0 i1) + ws-work-ep
                 
           display ' tax stuff ' am-account ' ' am-ret-st-tax-use

           if am-ret-st-tax-use = 'Y'
              move am-ret-p-e          to am-ret-st-tax-use
           end-if

           if am-ret-st-tax-use = ' ' or 'N'
              continue
           else
pemtst*       if am-ret-st-tax-use = 'P'
                 compute cur-prem-tax (i0 i1) =
                    cur-prem-tax (i0 i1) + ep-prm-tax
pemtst*        else
pemtst*           if am-ret-st-tax-use = 'E'
pemtst*              compute cur-prem-tax (i0 i1) =
pemtst*                 cur-prem-tax (i0 i1) + ws-work-ep /
pemtst*                 (ep-iss-prm - ep-cnc-prm) * ep-prm-tax
pemtst**             compute ws-work-factor rounded =
pemtst**                ws-work-ep / (ep-iss-prm - ep-cnc-prm)
pemtst**             compute cur-prem-tax (i0 i1) rounded =
pemtst**                cur-prem-tax (i0 i1) + (ws-work-factor *
pemtst**                ep-prm-tax)
pemtst*           end-if
pemtst*        end-if
           end-if

           compute cur-mort-resv (i0 i1) =
              cur-mort-resv (i0 i1) + ep-mort-resv
           compute cur-clm-resv (i0 i1) = cur-clm-resv (i0 i1) +
              (ep-clm-du + ep-clm-pv + ep-loss-resv + ep-clm-ibnr)
           compute cur-clm-pmts (i0 i1) =
              cur-clm-pmts (i0 i1) + ep-clm-amt

           if i1 = +1
              compute cur-ret-fees (i0 i1) rounded =
                 cur-ret-fees (i0 i1) + (ws-work-ep * am-lf-ret)
           else
              compute cur-ret-fees (i0 i1) rounded =
                 cur-ret-fees (i0 i1) + (ws-work-ep * am-ah-ret)
           end-if

           .
       1100-exit.
           exit.

       1200-process-commission.

           if ec-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           move +0                     to ws-work-ep
           if am-ret-p-e = 'E' or ' '
              perform varying a1 from +1 by +1 until a1 > +5
                 evaluate true
                    when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                       move ec-comm-78(a1) to ws-work-ep
                    when ws-ep-code = 'B' or 'K' or 'L'
                       move ec-comm-st(a1) to ws-work-ep
                    when ws-ep-code = 'P'
                       move ec-comm-pr(a1) to ws-work-ep
                    when ws-ep-code = 'M'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) + ec-comm-pr(a1)) * .50
                    when ws-ep-code = '1'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) * .3333) +
                          (ec-comm-pr(a1) * .6666)
                    when ws-ep-code = '2'
                       compute ws-work-ep rounded =
                          (ec-comm-pr(a1) * .80) +
                          (ec-comm-78(a1) * .20)
                    when other
                       display ' invalid ep code ' ws-ep-code ' '
                          ep-account
                 end-evaluate
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute cur-acct-comms (i0 i1) =
                       cur-acct-comms (i0 i1) + ws-work-ep
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute cur-ow-comms (i0 i1) =
                          cur-ow-comms (i0 i1) + ws-work-ep
                    end-if
                 end-if
              end-perform
           else
              perform varying a1 from +1 by +1 until a1 > +5
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute cur-acct-comms (i0 i1) =
                       cur-acct-comms (i0 i1) + (ec-iss-comm (a1) -
                       ec-cnc-comm (a1))
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute cur-ow-comms (i0 i1) =
                          cur-ow-comms (i0 i1) + (ec-iss-comm (a1) -
                          ec-cnc-comm (a1))
                    end-if
                 end-if
              end-perform
           end-if

           .
       1200-exit.
           exit.

       2000-accum-last-month-end.

           if ep-record-id = 'EP'
              perform 2100-process-premium
                                       thru 2100-exit
           else
              perform 2200-process-commission
                                       thru 2200-exit
           end-if

           .
       2000-exit.
           exit.

       2100-process-premium.

           if ep-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           compute lme-gross-prem (i0 i1) =
              lme-gross-prem (i0 i1) + ep-iss-prm
           compute lme-cancel-prem (i0 i1) =
              lme-cancel-prem (i0 i1) + ep-cnc-prm

           evaluate true
              when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                 move ep-prm-78     to ws-work-ep
              when ws-ep-code = 'B' or 'K' or 'L'
                 move ep-prm-st     to ws-work-ep
              when ws-ep-code = 'P'
                 move ep-prm-pr     to ws-work-ep
              when ws-ep-code = 'M'
                 compute ws-work-ep rounded =
                    (ep-prm-78 + ep-prm-pr) * .50
              when ws-ep-code = '1'
                 compute ws-work-ep rounded =
                    (ep-prm-78 * .3333) + (ep-prm-pr * .6666)
              when ws-ep-code = '2'
                 compute ws-work-ep rounded =
                    (ep-prm-pr * .80) + (ep-prm-78 * .20)
           end-evaluate

           compute lme-uep (i0 i1) = lme-uep (i0 i1) +
              (ep-iss-prm - ep-cnc-prm - ws-work-ep)
           compute lme-earn-prem (i0 i1) =
              lme-earn-prem (i0 i1) + ws-work-ep

           if am-ret-st-tax-use = 'Y'
              move am-ret-p-e          to am-ret-st-tax-use
           end-if

           if am-ret-st-tax-use = ' ' or 'N'
              continue
           else
      *       if am-ret-st-tax-use = 'P'
                 compute lme-prem-tax (i0 i1) =
                    lme-prem-tax (i0 i1) + ep-prm-tax
      *       else
      *          if am-ret-st-tax-use = 'E'
      *             compute lme-prem-tax (i0 i1) =
      *                lme-prem-tax (i0 i1) + ws-work-ep /
      *                (ep-iss-prm - ep-cnc-prm) * ep-prm-tax
      **            compute ws-work-factor rounded =
      **               ws-work-ep / (ep-iss-prm - ep-cnc-prm)
      **            compute lme-prem-tax (i0 i1) rounded =
      **               lme-prem-tax (i0 i1) + (ws-work-factor *
      **               ep-prm-tax)
      *          end-if
      *       end-if
           end-if

      *    compute lme-prem-tax (i0 i1) =
      *       lme-prem-tax (i0 i1) + ep-prm-tax
           compute lme-mort-resv (i0 i1) =
              lme-mort-resv (i0 i1) + ep-mort-resv
           compute lme-clm-resv (i0 i1) = lme-clm-resv (i0 i1) +
              (ep-clm-du + ep-clm-pv + ep-loss-resv + ep-clm-ibnr)
           compute lme-clm-pmts (i0 i1) =
              lme-clm-pmts (i0 i1) + ep-clm-amt

           if i1 = +1
              compute lme-ret-fees (i0 i1) rounded =
                 lme-ret-fees (i0 i1) + (ws-work-ep * am-lf-ret)
           else
              compute lme-ret-fees (i0 i1) rounded =
                 lme-ret-fees (i0 i1) + (ws-work-ep * am-ah-ret)
           end-if

           .
       2100-exit.
           exit.

       2200-process-commission.

           if ec-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           move +0                     to ws-work-ep
           if am-ret-p-e = 'E' or ' '
              perform varying a1 from +1 by +1 until a1 > +5
                 evaluate true
                    when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                       move ec-comm-78(a1) to ws-work-ep
                    when ws-ep-code = 'B' or 'K' or 'L'
                       move ec-comm-st(a1) to ws-work-ep
                    when ws-ep-code = 'P'
                       move ec-comm-pr(a1) to ws-work-ep
                    when ws-ep-code = 'M'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) + ec-comm-pr(a1)) * .50
                    when ws-ep-code = '1'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) * .3333) +
                          (ec-comm-pr(a1) * .6666)
                    when ws-ep-code = '2'
                       compute ws-work-ep rounded =
                          (ec-comm-pr(a1) * .80) +
                          (ec-comm-78(a1) * .20)
                    when other
                       display ' invalid ep code ' ws-ep-code ' '
                          ep-account
                 end-evaluate
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute lme-acct-comms (i0 i1) =
                       lme-acct-comms (i0 i1) + ws-work-ep
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute lme-ow-comms (i0 i1) =
                          lme-ow-comms (i0 i1) + ws-work-ep
                    end-if
                 end-if
              end-perform
           else
              perform varying a1 from +1 by +1 until a1 > +5
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute lme-acct-comms (i0 i1) =
                       lme-acct-comms (i0 i1) + (ec-iss-comm (a1) -
                       ec-cnc-comm (a1))
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute lme-ow-comms (i0 i1) =
                          lme-ow-comms (i0 i1) + (ec-iss-comm (a1) -
                          ec-cnc-comm (a1))
                    end-if
                 end-if
              end-perform
           end-if

           .
       2200-exit.
           exit.

       3000-accum-last-year-end.

           if ep-record-id = 'EP'
              perform 3100-process-premium
                                       thru 3100-exit
           else
              perform 3200-process-commission
                                       thru 3200-exit
           end-if

           .
       3000-exit.
           exit.

       3100-process-premium.

           if ep-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           compute lye-gross-prem (i0 i1) =
              lye-gross-prem (i0 i1) + ep-iss-prm
           compute lye-cancel-prem (i0 i1) =
              lye-cancel-prem (i0 i1) + ep-cnc-prm

           evaluate true
              when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                 move ep-prm-78     to ws-work-ep
              when ws-ep-code = 'B' or 'K' or 'L'
                 move ep-prm-st     to ws-work-ep
              when ws-ep-code = 'P'
                 move ep-prm-pr     to ws-work-ep
              when ws-ep-code = 'M'
                 compute ws-work-ep rounded =
                    (ep-prm-78 + ep-prm-pr) * .50
              when ws-ep-code = '1'
                 compute ws-work-ep rounded =
                    (ep-prm-78 * .3333) + (ep-prm-pr * .6666)
              when ws-ep-code = '2'
                 compute ws-work-ep rounded =
                    (ep-prm-pr * .80) + (ep-prm-78 * .20)
           end-evaluate

           compute lye-uep (i0 i1) = lye-uep (i0 i1) +
              (ep-iss-prm - ep-cnc-prm - ws-work-ep)
           compute lye-earn-prem (i0 i1) =
              lye-earn-prem (i0 i1) + ws-work-ep

           if am-ret-st-tax-use = 'Y'
              move am-ret-p-e          to am-ret-st-tax-use
           end-if

           if am-ret-st-tax-use = ' ' or 'N'
              continue
           else
      *       if am-ret-st-tax-use = 'P'
                 compute lye-prem-tax (i0 i1) =
                    lye-prem-tax (i0 i1) + ep-prm-tax
      *       else
      *          if am-ret-st-tax-use = 'E'
      *             compute lye-prem-tax (i0 i1) =
      *                lye-prem-tax (i0 i1) + ws-work-ep /
      *                (ep-iss-prm - ep-cnc-prm) * ep-prm-tax
      **            compute ws-work-factor rounded =
      **               ws-work-ep / (ep-iss-prm - ep-cnc-prm)
      **            compute lye-prem-tax (i0 i1) rounded =
      **               lye-prem-tax (i0 i1) + (ws-work-factor *
      **               ep-prm-tax)
      *          end-if
      *       end-if
           end-if

      *    compute lye-prem-tax (i0 i1) =
      *       lye-prem-tax (i0 i1) + ep-prm-tax
           compute lye-mort-resv (i0 i1) =
              lye-mort-resv (i0 i1) + ep-mort-resv
           compute lye-clm-resv (i0 i1) = lye-clm-resv (i0 i1) +
              (ep-clm-du + ep-clm-pv + ep-loss-resv + ep-clm-ibnr)
           compute lye-clm-pmts (i0 i1) =
              lye-clm-pmts (i0 i1) + ep-clm-amt

           if i1 = +1
              compute lye-ret-fees (i0 i1) rounded =
                 lye-ret-fees (i0 i1) + (ws-work-ep * am-lf-ret)
           else
              compute lye-ret-fees (i0 i1) rounded =
                 lye-ret-fees (i0 i1) + (ws-work-ep * am-ah-ret)
           end-if

           .
       3100-exit.
           exit.

       3200-process-commission.

           if ec-rcd-type = 'L' or 'P'
              move +1                  to i1
           else
              move +2                  to i1
           end-if

           move +0                     to ws-work-ep
           if am-ret-p-e = 'E' or ' '
              perform varying a1 from +1 by +1 until a1 > +5
                 evaluate true
                    when ws-ep-code = 'R' OR 'N' OR 'U' OR 'A' OR 'T'
                       move ec-comm-78(a1) to ws-work-ep
                    when ws-ep-code = 'B' or 'K' or 'L'
                       move ec-comm-st(a1) to ws-work-ep
                    when ws-ep-code = 'P'
                       move ec-comm-pr(a1) to ws-work-ep
                    when ws-ep-code = 'M'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) + ec-comm-pr(a1)) * .50
                    when ws-ep-code = '1'
                       compute ws-work-ep rounded =
                          (ec-comm-78(a1) * .3333) +
                          (ec-comm-pr(a1) * .6666)
                    when ws-ep-code = '2'
                       compute ws-work-ep rounded =
                          (ec-comm-pr(a1) * .80) +
                          (ec-comm-78(a1) * .20)
                    when other
                       display ' invalid ep code ' ws-ep-code ' '
                          ep-account
                 end-evaluate
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute lye-acct-comms (i0 i1) =
                       lye-acct-comms (i0 i1) + ws-work-ep
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute lye-ow-comms (i0 i1) =
                          lye-ow-comms (i0 i1) + ws-work-ep
                    end-if
                 end-if
              end-perform
           else
              perform varying a1 from +1 by +1 until a1 > +5
                 if ec-agt-type (a1) = 'C' or 'D'
                    compute lye-acct-comms (i0 i1) =
                       lye-acct-comms (i0 i1) + (ec-iss-comm (a1) -
                       ec-cnc-comm (a1))
                 else
                    if ec-agt-type (a1) = 'O' or 'P'
                       compute lye-ow-comms (i0 i1) =
                          lye-ow-comms (i0 i1) + (ec-iss-comm (a1)
                          - ec-cnc-comm (a1))
                    end-if
                 end-if
              end-perform
           end-if

           .
       3200-exit.
           exit.

       3300-release-rec.

      *    if (am-ret-y-n = 'Y')
      *       and (am-report-code-3 = 'SVCLIFE')
      *       continue
      *    else
      *       go to 3300-continue
      *    end-if

           if zero = cur-gross-prem (1 1)
                 and cur-uep        (1 1)
                 and cur-acct-comms (1 1)
                 and cur-clm-pmts   (1 1)
                 and cur-clm-resv   (1 1)
                 and cur-gross-prem (1 2)
                 and cur-uep        (1 2)
                 and cur-acct-comms (1 2)
                 and cur-clm-pmts   (1 2)
                 and cur-clm-resv   (1 2)
              go to 3300-continue
           end-if

           move ws-acct-ret-grp        to sw-retro-group
           move ws-prev-acct-key       to sw-acct-key
           move ws-acct-name           to sw-acct-name
           move ws-acct-addr2          to sw-acct-addr1
           move ws-acct-city-state     to sw-acct-city-state
           move ws-acct-zip            to sw-acct-zip
           move ws-acct-rptcd1         to sw-acct-rptcd1
           move ws-acct-rptcd2         to sw-acct-rptcd2
           move ws-acct-rptcd3         to sw-acct-rptcd3

pemtst**             compute ws-work-factor rounded =
pemtst**                ws-work-ep / (ep-iss-prm - ep-cnc-prm)
pemtst**             compute cur-prem-tax (i0 i1) rounded =
pemtst**                cur-prem-tax (i0 i1) + (ws-work-factor *
pemtst**                ep-prm-tax)



           if am-ret-st-tax-use = 'E'
              compute ws-work-factor rounded = cur-earn-prem (1 1) /
                 (cur-gross-prem (1 1) - cur-cancel-prem (1 1))
              compute cur-prem-tax (1 1) rounded =
                 ws-work-factor * cur-prem-tax (1 1)
              compute ws-work-factor rounded = cur-earn-prem (1 2) /
                 (cur-gross-prem (1 2) - cur-cancel-prem (1 2))
              compute cur-prem-tax (1 2) rounded =
                 ws-work-factor * cur-prem-tax (1 2)

              compute ws-work-factor rounded = lme-earn-prem (1 1) /
                 (lme-gross-prem (1 1) - lme-cancel-prem (1 1))
              compute lme-prem-tax (1 1) rounded =
                 ws-work-factor * lme-prem-tax (1 1)
              compute ws-work-factor rounded = lme-earn-prem (1 2) /
                 (lme-gross-prem (1 2) - lme-cancel-prem (1 2))
              compute lme-prem-tax (1 2) rounded =
                 ws-work-factor * lme-prem-tax (1 2)

              compute ws-work-factor rounded = lye-earn-prem (1 1) /
                 (lye-gross-prem (1 1) - lye-cancel-prem (1 1))
              compute lye-prem-tax (1 1) rounded =
                 ws-work-factor * lye-prem-tax (1 1)
              compute ws-work-factor rounded = lye-earn-prem (1 2) /
                 (lye-gross-prem (1 2) - lye-cancel-prem (1 2))
              compute lye-prem-tax (1 2) rounded =
                 ws-work-factor * lye-prem-tax (1 2)
           end-if

      *       compute cur-prem-tax (1 1) = cur-earn-prem (1 1) /
      *          (cur-gross-prem (1 1) - cur-cancel-prem (1 1)) *
      *          cur-prem-tax (1 1)
      *       compute cur-prem-tax (1 2) = cur-earn-prem (1 2) /
      *          (cur-gross-prem (1 2) - cur-cancel-prem (1 2)) *
      *          cur-prem-tax (1 2)
      *
      *       compute lme-prem-tax (1 1) = lme-earn-prem (1 1) /
      *          (lme-gross-prem (1 1) - lme-cancel-prem (1 1)) *
      *          lme-prem-tax (1 1)
      *       compute lme-prem-tax (1 2) = lme-earn-prem (1 2) /
      *          (lme-gross-prem (1 2) - lme-cancel-prem (1 2)) *
      *          lme-prem-tax (1 2)
      *
      *       compute lye-prem-tax (1 1) = lye-earn-prem (1 1) /
      *          (lye-gross-prem (1 1) - lye-cancel-prem (1 1)) *
      *          lye-prem-tax (1 1)
      *       compute lye-prem-tax (1 2) = lye-earn-prem (1 2) /
      *          (lye-gross-prem (1 2) - lye-cancel-prem (1 2)) *
      *          lye-prem-tax (1 2)
      *    end-if

      *    compute cur-ret-fees (1) = cur-earn-prem (1) * am-lf-ret
      *    compute cur-ret-fees (2) = cur-earn-prem (2) * am-ah-ret
      *    compute lme-ret-fees (1) = lme-earn-prem (1) * am-lf-ret
      *    compute lme-ret-fees (2) = lme-earn-prem (2) * am-ah-ret
      *    compute lye-ret-fees (1) = lye-earn-prem (1) * am-lf-ret
      *    compute lye-ret-fees (2) = lye-earn-prem (2) * am-ah-ret

      ***  first occurrence is account date range detail

           move ws-accum-record (1)    to sw-accums
           display ' key ' sw-retro-group ' ' sw-acct-key
           release sw-rec-out

      *    add +1 to d1
      *    move sw-rec-out             to ws-hold-dr-records (d1)
      *    move d1                     to d1m
      *    display ' just released ' sw-retro-group ' ' sw-acct-key
      *       ' ' SW-EXP-DTE ' ' d1m

           add 1 to ws-rel-recs

           .
       3300-continue.

           move ep-carrier             to ws-prev-carrier
           move ep-grouping            to ws-prev-grouping
           move ep-state               to ws-prev-state
           move ep-account             to ws-prev-account
           move ep-exp-dte             to ws-prev-exp-dte
           move +1                     to i0
           perform 0050-init-acct-accums
                                       thru 0050-exit

           .
       3300-exit.
           exit.

       3500-release-drs.

           perform 3300-release-rec    thru 3300-exit

           if am-ret-y-n not = ' '
              perform varying d1 from +1 by +1 until d1 > d1m
                 move ws-hold-dr-records (d1)
                                       to sw-rec-out
                 release sw-rec-out
                 add 1 to ws-rel-drs
              end-perform
           else
              if d1m not = +0
                 display ' bypassing account ' ws-prev-carrier ' '
                    ws-prev-state ' ' ws-prev-account
              end-if
           end-if

           .
       3500-exit.
           exit.


       4000-sort-output.

           perform 4100-init           thru 4100-exit
           perform 4300-process-output thru 4300-exit until
              end-of-input

           perform 4330-final-totals   thru 4330-exit

           .
       4000-exit.
           exit.

       4100-init.

           move ' '                    to ws-eof-sw
           perform varying i0 from +1 by +1 until i0 > +4
              perform 0050-init-acct-accums
                                       thru 0050-exit
           end-perform

           perform 4200-return         thru 4200-exit
           move sw-retro-group         to ws-prev-retro-group
           move sw-acct-key            to ws-prev-acct-key

           .
       4100-exit.
           exit.

       4200-return.

           return sort-work at end 
              set end-of-input to true
           end-return

           if not end-of-input
              move sw-accums           to ws-accum-record (1)
              display ' ret key ' sw-retro-group ' ' sw-acct-key
           end-if

           .
       4200-exit.
           exit.


       4300-process-output.

           if sw-retro-group not = ws-prev-retro-group
              perform 4320-ret-grp-break
                                       thru 4320-exit
           end-if

           if sw-acct-key not = ws-prev-acct-key
              perform 4310-acct-break  thru 4310-exit
           end-if

      *****   adding date range detail to account level totals

           move +1                     to i9
           move +2                     to o9
           perform 4500-accum-levels   thru 4500-exit

           move sw-retro-group         to ws-prev-retro-group
           move sw-acct-key            to ws-prev-acct-key
           move sw-acct-name           to ws-acct-name
           move sw-acct-addr1          to ws-acct-addr1
           move sw-acct-city-state     to ws-acct-city-state
           move sw-acct-zip            to ws-acct-zip
           move sw-acct-rptcd1         to ws-acct-rptcd1
           move sw-acct-rptcd2         to ws-acct-rptcd2
           move sw-acct-rptcd3         to ws-acct-rptcd3

           perform 4200-return         thru 4200-exit

           .
       4300-exit.
           exit.

       4310-acct-break.

      ***  print account tots
           move +2                     to i0
           perform 4400-print-totals   thru 4400-exit

      ***  add acct tots to retro group tots
           move +2                     to i9
           move +3                     to o9
           perform 4500-accum-levels   thru 4500-exit

      ***  zero out the account accums
           move +2                     to i0
           perform 0050-init-acct-accums
                                       thru 0050-exit
           .
       4310-exit.
           exit.

       4320-ret-grp-break.

           perform 4310-acct-break     thru 4310-exit
      ***  print retro group tots
           move +3                     to i0
           perform 4400-print-totals   thru 4400-exit

      ***  add retro group tots to final tots
           move +3                     to i9
           move +4                     to o9
           perform 4500-accum-levels   thru 4500-exit

      ***  zero out the retro group accums
           move +3                     to i0
           perform 0050-init-acct-accums
                                       thru 0050-exit

           .
       4320-exit.
           exit.

       4330-final-totals.

           perform 4310-acct-break     thru 4310-exit
      ***  print retro group tots
           move +3                     to i0
           perform 4400-print-totals   thru 4400-exit

      ***  add retro group tots to final tots
           move +3                     to i9
           move +4                     to o9
           perform 4500-accum-levels   thru 4500-exit

      ***  zero out the retro group accums
           move +3                     to i0
           perform 0050-init-acct-accums
                                       thru 0050-exit

           move +4                     to i0
           perform 4400-print-totals   thru 4400-exit

           .
       4330-exit.
           exit.

       4400-print-totals.

           move ws-prev-retro-group    to h5-group
                                          ir-pool-code
      *    if ws-prev-retro-group = 'MISAUT'
      *       set active-group to true
      *       move 'MISAUT'            to h5-name
      *    else
           search all ws-retro-table at end
              continue
             when ws-prev-retro-group = ws-retro-group (r1)
              set active-group to true
              move ws-retro-name (r1)
                                       to h5-name
              move ws-retro-eff-dte (r1)
                                       to h4-eff-dte
              move ws-retro-next-rpt-mo (r1)
                                       to h5-anniv-dte
              move '/01'               to h5-anniv-dte (3:3)
           end-search
      *    end-if
           if i0 = +3 or +4
              move spaces              to h1-name
                                          h2-addr
                                          h3-addr
      *                                   h3-zip
                                          h5-st
                                          h5-acct
           else
              move ws-acct-name        to h1-name
              move ws-acct-addr1       to h2-addr

              move spaces              to h3-addr
              if ws-acct-zip (6:4) not = spaces
                 string
                    ws-acct-city-state ' '
                    ws-acct-zip (1:5)  '-'
                    ws-acct-zip (6:4)
                    delimited by '  '     into h3-addr
                 end-string
              else
                 string
                    ws-acct-city-state ' '
                    ws-acct-zip (1:5)
                    delimited by '  '     into h3-addr
                 end-string
              end-if
      *       move ws-acct-city-state  to h3-addr
      *       move ws-acct-zip         to h3-zip
              move 'ST ACCOUNT  '      to h4-stacct
              move ws-prev-carrier     to ir-carrier
              move ws-prev-grouping    to ir-group
              move ws-prev-state       to h5-st
                                          ir-state
              move ws-prev-account     to h5-acct
                                          ir-account
              move ws-acct-rptcd1      to ir-rptcd1
              move ws-acct-rptcd2      to ir-rptcd2
              move ws-acct-rptcd3      to ir-rptcd3
              move '-'                 to H5-FS1
      *       move ws-prev-retro-group to h5-group
           end-if

           if i0 = +3
              move 'RETRO GROUP TOTALS ' TO H3-ADDR
              move spaces                to H5-FS1
                                            H4-STACCT
           end-if
           if i0 =  +4
              move spaces              to h5-group
                                          h5-name
                                          head-4
                                          H5-FS1
              move 'FINAL TOTALS '     to h3-addr
           end-if

      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   This code creates an index file that is used by the      ***
      ***   RDS for report distribution.                             ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

           compute ir-page-no = ir-page-no + 1

           evaluate true
              when i0 = +4
                 move spaces to index-record (6:55)
              when i0 = +3
                 move spaces to index-record (12:49)
           end-evaluate

           string
              ir-page-no               ';'
              ir-pool-code             ';'
              ir-rptcd1                ';'
              ir-rptcd2                ';'
              ir-rptcd3                ';'
              ir-carrier               ';'
              ir-group                 ';'
              ir-state                 ';'
              ir-account               ';'
              delimited by size into index-file-rec
           end-string
           write index-file-rec

           perform 0590-heading-routine thru 0590-exit

      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   B E G I N   R E V E N U E S                              ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

           compute cur-net-prem (i0 1) =
              cur-gross-prem (i0 1) - cur-cancel-prem (i0 1)
           compute cur-earn-prem (i0 1) =
              cur-net-prem (i0 1) - cur-uep (i0 1)
           compute cur-tot-income (i0 1) =
              cur-earn-prem (i0 1) + cur-inv-income (i0 1)

           compute cur-net-prem (i0 2) =
              cur-gross-prem (i0 2) - cur-cancel-prem (i0 2)
           compute cur-earn-prem (i0 2) =
              cur-net-prem (i0 2) - cur-uep (i0 2)
           compute cur-tot-income (i0 2) =
              cur-earn-prem (i0 2) + cur-inv-income (i0 2)

           compute lme-net-prem (i0 1) =
              lme-gross-prem (i0 1) - lme-cancel-prem (i0 1)
           compute lme-earn-prem (i0 1) =
              lme-net-prem (i0 1) - lme-uep (i0 1)
           compute lme-tot-income (i0 1) =
              lme-earn-prem (i0 1) + lme-inv-income (i0 1)

           compute lme-net-prem (i0 2) =
              lme-gross-prem (i0 2) - lme-cancel-prem (i0 2)
           compute lme-earn-prem (i0 2) =
              lme-net-prem (i0 2) - lme-uep (i0 2)
           compute lme-tot-income (i0 2) =
              lme-earn-prem (i0 2) + lme-inv-income (i0 2)

           compute lye-net-prem (i0 1) =
              lye-gross-prem (i0 1) - lye-cancel-prem (i0 1)
           compute lye-earn-prem (i0 1) =
              lye-net-prem (i0 1) - lye-uep (i0 1)
           compute lye-tot-income (i0 1) =
              lye-earn-prem (i0 1) + lye-inv-income (i0 1)

           compute lye-net-prem (i0 2) =
              lye-gross-prem (i0 2) - lye-cancel-prem (i0 2)
           compute lye-earn-prem (i0 2) =
              lye-net-prem (i0 2) - lye-uep (i0 2)
           compute lye-tot-income (i0 2) =
              lye-earn-prem (i0 2) + lye-inv-income (i0 2)


           move '      Gross Premium           '
                                       to det-desc
           compute det-mtd-lf =
              cur-gross-prem (i0 1) - lme-gross-prem (i0 1)
           compute det-mtd-ah =
              cur-gross-prem (i0 2) - lme-gross-prem (i0 2)
           compute det-ytd-lf =
              cur-gross-prem (i0 1) - lye-gross-prem (i0 1)
           compute det-ytd-ah =
              cur-gross-prem (i0 2) - lye-gross-prem (i0 2)
           move cur-gross-prem (i0 1)     to det-itd-lf
           move cur-gross-prem (i0 2)     to det-itd-ah
           compute det-itd-tot =
              cur-gross-prem (i0 1) + cur-gross-prem (i0 2)
           MOVE '0'                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '      Cancellations           '
                                       to det-desc
           compute det-mtd-lf =
              cur-cancel-prem (i0 1) - lme-cancel-prem (i0 1)
           compute det-mtd-ah =
              cur-cancel-prem (i0 2) - lme-cancel-prem (i0 2)
           compute det-ytd-lf =
              cur-cancel-prem (i0 1) - lye-cancel-prem (i0 1)
           compute det-ytd-ah =
              cur-cancel-prem (i0 2) - lye-cancel-prem (i0 2)
           move cur-cancel-prem (i0 1) to det-itd-lf
           move cur-cancel-prem (i0 2) to det-itd-ah
           compute det-itd-tot =
              cur-cancel-prem (i0 1) + cur-cancel-prem (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move 'Net Written Premium           '
                                       to det-desc
           compute det-mtd-lf =
              cur-net-prem (i0 1) - lme-net-prem (i0 1)
           compute det-mtd-ah =
              cur-net-prem (i0 2) - lme-net-prem (i0 2)
           compute det-ytd-lf =
              cur-net-prem (i0 1) - lye-net-prem (i0 1)
           compute det-ytd-ah =
              cur-net-prem (i0 2) - lye-net-prem (i0 2)
           move cur-net-prem (i0 1)    to det-itd-lf
           move cur-net-prem (i0 2)    to det-itd-ah
           compute det-itd-tot =
              cur-net-prem (i0 1) + cur-net-prem (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '      Change in UEP           '
                                       to det-desc
           compute det-mtd-lf =
              cur-uep (i0 1) - lme-uep (i0 1)
           compute det-mtd-ah =
              cur-uep (i0 2) - lme-uep (i0 2)
           compute det-ytd-lf =
              cur-uep (i0 1) - lye-uep (i0 1)
           compute det-ytd-ah =
              cur-uep (i0 2) - lye-uep (i0 2)
           move cur-uep (i0 1)         to det-itd-lf
           move cur-uep (i0 2)         to det-itd-ah
           compute det-itd-tot =
              cur-uep (i0 1) + cur-uep (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT


           move '     Earned Premium           '
                                       to det-desc
           compute det-mtd-lf =
              cur-earn-prem (i0 1) - lme-earn-prem (i0 1)
           compute det-mtd-ah =
              cur-earn-prem (i0 2) - lme-earn-prem (i0 2)
           compute det-ytd-lf =
              cur-earn-prem (i0 1) - lye-earn-prem (i0 1)
           compute det-ytd-ah =
              cur-earn-prem (i0 2) - lye-earn-prem (i0 2)
           move cur-earn-prem (i0 1)   to det-itd-lf
           move cur-earn-prem (i0 2)   to det-itd-ah
           compute det-itd-tot =
              cur-earn-prem (i0 1) + cur-earn-prem (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      *    move '  Investment Income           '
      *                                to det-desc
      *    compute det-mtd-lf =
      *       cur-inv-income (i0 1) - lme-inv-income (i0 1)
      *    compute det-mtd-ah =
      *       cur-inv-income (i0 2) - lme-inv-income (i0 2)
      *    compute det-ytd-lf =
      *       cur-inv-income (i0 1) - lye-inv-income (i0 1)
      *    compute det-ytd-ah =
      *       cur-inv-income (i0 2) - lye-inv-income (i0 2)
      *    move cur-inv-income (i0 1)     to det-itd-lf
      *    move cur-inv-income (i0 2)     to det-itd-ah
      *    MOVE ' '                    TO X
      *    MOVE detail-line            TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

jeffw *    move '     Total Revenues           '
jeffw *                                to det-desc
jeffw *    compute det-mtd-lf =
jeffw *       cur-tot-income (i0 1) - lme-tot-income (i0 1)
jeffw *    compute det-mtd-ah =
jeffw *       cur-tot-income (i0 2) - lme-tot-income (i0 2)
jeffw *    compute det-ytd-lf =
jeffw *       cur-tot-income (i0 1) - lye-tot-income (i0 1)
jeffw *    compute det-ytd-ah =
jeffw *       cur-tot-income (i0 2) - lye-tot-income (i0 2)
jeffw *    move cur-tot-income (i0 1)     to det-itd-lf
jeffw *    move cur-tot-income (i0 2)     to det-itd-ah
jeffw *    MOVE ' '                    TO X
jeffw *    MOVE detail-line            TO P-DATA
jeffw *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   B E G I N   E X P E N S E S                              ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

           compute cur-tot-expenses (i0 1) =
              cur-acct-comms (i0 1) + cur-ret-fees (i0 1) +
              cur-clm-pmts (i0 1)
              + cur-prem-tax (i0 1) + cur-other-expenses (i0 1)
           compute cur-tot-expenses (i0 2) =
              cur-acct-comms (i0 2) + cur-ret-fees (i0 2) +
               cur-clm-pmts (i0 2)
              + cur-prem-tax (i0 2) + cur-other-expenses (i0 2)

           compute lme-tot-expenses (i0 1) =
              lme-acct-comms (i0 1) + lme-ret-fees (i0 1) +
               lme-clm-pmts (i0 1)
              + lme-prem-tax (i0 1) + lme-other-expenses (i0 1)
           compute lme-tot-expenses (i0 2) =
              lme-acct-comms (i0 2) + lme-ret-fees (i0 2) +
               lme-clm-pmts (i0 2)
              + lme-prem-tax (i0 2) + lme-other-expenses (i0 2)

           compute lye-tot-expenses (i0 1) =
              lye-acct-comms (i0 1) + lye-ret-fees (i0 1) +
               lye-clm-pmts (i0 1)
              + lye-prem-tax (i0 1) + lye-other-expenses (i0 1)
           compute lye-tot-expenses (i0 2) =
              lye-acct-comms (i0 2) + lye-ret-fees (i0 2) +
               lye-clm-pmts (i0 2)
              + lye-prem-tax (i0 2) + lye-other-expenses (i0 2)

           compute cur-net-cash-flow (i0 1) =
              cur-tot-income (i0 1) - cur-tot-expenses (i0 1)
           compute cur-net-cash-flow (i0 2) =
              cur-tot-income (i0 2) - cur-tot-expenses (i0 2)

           compute lme-net-cash-flow (i0 1) =
              lme-tot-income (i0 1) - lme-tot-expenses (i0 1)
           compute lme-net-cash-flow (i0 2) =
              lme-tot-income (i0 2) - lme-tot-expenses (i0 2)

           compute lye-net-cash-flow (i0 1) =
              lye-tot-income (i0 1) - lye-tot-expenses (i0 1)
           compute lye-net-cash-flow (i0 2) =
              lye-tot-income (i0 2) - lye-tot-expenses (i0 2)

      *    compute cur-net-mep-comm (i0 1) = cur-net-cash-flow (i0 1) -
      *       cur-mort-resv (i0 1) - cur-clm-resv (i0 1)
      *    compute cur-net-mep-comm (i0 2) = cur-net-cash-flow (i0 2) -
      *       cur-mort-resv (i0 2) - cur-clm-resv (i0 2)
      *    compute lme-net-mep-comm (i0 1) = lme-net-cash-flow (i0 1) -
      *       lme-mort-resv (i0 1) - lme-clm-resv (i0 1)
      *    compute lme-net-mep-comm (i0 2) = lme-net-cash-flow (i0 2) -
      *       lme-mort-resv (i0 2) - lme-clm-resv (i0 2)
      *    compute lye-net-mep-comm (i0 1) = lye-net-cash-flow (i0 1) -
      *       lye-mort-resv (i0 1) - lye-clm-resv (i0 1)
      *    compute lye-net-mep-comm (i0 2) = lye-net-cash-flow (i0 2) -
      *       lye-mort-resv (i0 2) - lye-clm-resv (i0 2)

           compute cur-net-mep-comm (i0 1) =
              cur-net-cash-flow (i0 1) - cur-clm-resv (i0 1)
           compute cur-net-mep-comm (i0 2) =
              cur-net-cash-flow (i0 2) - cur-clm-resv (i0 2)
           compute lme-net-mep-comm (i0 1) =
              lme-net-cash-flow (i0 1) - lme-clm-resv (i0 1)
           compute lme-net-mep-comm (i0 2) =
              lme-net-cash-flow (i0 2) - lme-clm-resv (i0 2)
           compute lye-net-mep-comm (i0 1) =
              lye-net-cash-flow (i0 1) - lye-clm-resv (i0 1)
           compute lye-net-mep-comm (i0 2) =
              lye-net-cash-flow (i0 2) - lye-clm-resv (i0 2)

           compute cur-mep-due (i0 1) =
              cur-net-mep-comm (i0 1) - cur-retro-pmts (i0 1)
           compute cur-mep-due (i0 2) =
              cur-net-mep-comm (i0 2) - cur-retro-pmts (i0 2)
           compute lme-mep-due (i0 1) =
              lme-net-mep-comm (i0 1) - lme-retro-pmts (i0 1)
           compute lme-mep-due (i0 2) =
              lme-net-mep-comm (i0 2) - lme-retro-pmts (i0 2)
           compute lye-mep-due (i0 1) =
              lye-net-mep-comm (i0 1) - lye-retro-pmts (i0 1)
           compute lye-mep-due (i0 2) =
              lye-net-mep-comm (i0 2) - lye-retro-pmts (i0 2)

           MOVE '-'                    TO X
           MOVE spaces                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT


      *    MOVE '0'                    TO X
      *    MOVE HEAD-9                 TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '        Commissions           '
                                       to det-desc
           compute det-mtd-lf =
              cur-acct-comms (i0 1) - lme-acct-comms (i0 1)
           compute det-mtd-ah =
              cur-acct-comms (i0 2) - lme-acct-comms (i0 2)
           compute det-ytd-lf =
              cur-acct-comms (i0 1) - lye-acct-comms (i0 1)
           compute det-ytd-ah =
              cur-acct-comms (i0 2) - lye-acct-comms (i0 2)
           move cur-acct-comms (i0 1)  to det-itd-lf
           move cur-acct-comms (i0 2)  to det-itd-ah
           compute det-itd-tot =
              cur-acct-comms (i0 1) + cur-acct-comms (i0 2)
           MOVE '0'                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '     Retention Fees           '
                                       to det-desc
           compute det-mtd-lf =
              cur-ret-fees (i0 1) - lme-ret-fees (i0 1)
           compute det-mtd-ah =
              cur-ret-fees (i0 2) - lme-ret-fees (i0 2)
           compute det-ytd-lf =
              cur-ret-fees (i0 1) - lye-ret-fees (i0 1)
           compute det-ytd-ah =
              cur-ret-fees (i0 2) - lye-ret-fees (i0 2)
           move cur-ret-fees (i0 1)    to det-itd-lf
           move cur-ret-fees (i0 2)    to det-itd-ah
           compute det-itd-tot =
              cur-ret-fees (i0 1) + cur-ret-fees (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '        Claims Paid           '
                                       to det-desc
           compute det-mtd-lf =
              cur-clm-pmts (i0 1) - lme-clm-pmts (i0 1)
           compute det-mtd-ah =
              cur-clm-pmts (i0 2) - lme-clm-pmts (i0 2)
           compute det-ytd-lf =
              cur-clm-pmts (i0 1) - lye-clm-pmts (i0 1)
           compute det-ytd-ah =
              cur-clm-pmts (i0 2) - lye-clm-pmts (i0 2)
           move cur-clm-pmts (i0 1)    to det-itd-lf
           move cur-clm-pmts (i0 2)    to det-itd-ah
           compute det-itd-tot =
              cur-clm-pmts (i0 1) + cur-clm-pmts (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '      Premium Taxes           '
                                       to det-desc
           compute det-mtd-lf =
              cur-prem-tax (i0 1) - lme-prem-tax (i0 1)
           compute det-mtd-ah =
              cur-prem-tax (i0 2) - lme-prem-tax (i0 2)
           compute det-ytd-lf =
              cur-prem-tax (i0 1) - lye-prem-tax (i0 1)
           compute det-ytd-ah =
              cur-prem-tax (i0 2) - lye-prem-tax (i0 2)
           move cur-prem-tax (i0 1)    to det-itd-lf
           move cur-prem-tax (i0 2)    to det-itd-ah
           compute det-itd-tot =
              cur-prem-tax (i0 1) + cur-prem-tax (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '              Other           '
                                       to det-desc
           compute det-mtd-lf =
              cur-other-expenses (i0 1) - lme-other-expenses (i0 1)
           compute det-mtd-ah =
              cur-other-expenses (i0 2) - lme-other-expenses (i0 2)
           compute det-ytd-lf =
              cur-other-expenses (i0 1) - lye-other-expenses (i0 1)
           compute det-ytd-ah =
              cur-other-expenses (i0 2) - lye-other-expenses (i0 2)
           move cur-other-expenses (i0 1)
                                       to det-itd-lf
           move cur-other-expenses (i0 2)
                                       to det-itd-ah
           compute det-itd-tot =
              cur-other-expenses (i0 1) + cur-other-expenses (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '     Total Expenses           '
                                       to det-desc
           compute det-mtd-lf =
              cur-tot-expenses (i0 1) - lme-tot-expenses (i0 1)
           compute det-mtd-ah =
              cur-tot-expenses (i0 2) - lme-tot-expenses (i0 2)
           compute det-ytd-lf =
              cur-tot-expenses (i0 1) - lye-tot-expenses (i0 1)
           compute det-ytd-ah =
              cur-tot-expenses (i0 2) - lye-tot-expenses (i0 2)
           move cur-tot-expenses (i0 1) to det-itd-lf
           move cur-tot-expenses (i0 2) to det-itd-ah
           compute det-itd-tot =
              cur-tot-expenses (i0 1) + cur-tot-expenses (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

jeffw *    move ' N e t  C a s h  F l o w      '
jeffw *                                to det-desc
jeffw *    compute det-mtd-lf =
jeffw *       cur-net-cash-flow (i0 1) - lme-net-cash-flow (i0 1)
jeffw *    compute det-mtd-ah =
jeffw *       cur-net-cash-flow (i0 2) - lme-net-cash-flow (i0 2)
jeffw *    compute det-ytd-lf =
jeffw *       cur-net-cash-flow (i0 1) - lye-net-cash-flow (i0 1)
jeffw *    compute det-ytd-ah =
jeffw *       cur-net-cash-flow (i0 2) - lye-net-cash-flow (i0 2)
jeffw *    move cur-net-cash-flow (i0 1)     to det-itd-lf
jeffw *    move cur-net-cash-flow (i0 2)     to det-itd-ah
jeffw *    MOVE '0'                    TO X
jeffw *    MOVE detail-line            TO P-DATA
jeffw *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      *    move 'Change In Mort Reserve        '
      *                                to det-desc
      *    compute det-mtd-lf =
      *       cur-mort-resv (i0 1) - lme-mort-resv (i0 1)
      *    compute det-mtd-ah =
      *       cur-mort-resv (i0 2) - lme-mort-resv (i0 2)
      *    compute det-ytd-lf =
      *       cur-mort-resv (i0 1) - lye-mort-resv (i0 1)
      *    compute det-ytd-ah =
      *       cur-mort-resv (i0 2) - lye-mort-resv (i0 2)
      *    move cur-mort-resv (i0 1)     to det-itd-lf
      *    move cur-mort-resv (i0 2)     to det-itd-ah
      *    MOVE '0'                    TO X
      *    MOVE detail-line            TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '    Change In Claim           '
                                       to det-misc-desc
           MOVE '0'                    TO X
           MOVE detail-desc-line       TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT
           move '            Reserve           '
                                       to det-desc
           compute det-mtd-lf =
              cur-clm-resv (i0 1) - lme-clm-resv (i0 1)
           compute det-mtd-ah =
              cur-clm-resv (i0 2) - lme-clm-resv (i0 2)
           compute det-ytd-lf =
              cur-clm-resv (i0 1) - lye-clm-resv (i0 1)
           compute det-ytd-ah =
              cur-clm-resv (i0 2) - lye-clm-resv (i0 2)
           move cur-clm-resv (i0 1)    to det-itd-lf
           move cur-clm-resv (i0 2)    to det-itd-ah
           compute det-itd-tot =
              cur-clm-resv (i0 1) + cur-clm-resv (i0 2)
           MOVE ' '                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      *    move '   System Generated           '
      *                                to det-misc-desc
      *    MOVE '0'                    TO X
      *    MOVE detail-desc-line       TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT
      *    move '   Retro Commission           '
      *                                to det-desc

jeffw *    move ' Net MEP Commission           '
jeffw *                                to det-desc


           MOVE '-'                    TO X
           MOVE spaces                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT
           MOVE '0'                    TO X
           MOVE spaces                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move spaces                 to det-desc
jeffw      move '   System Generated           '
jeffw                                  to det-desc
           compute det-mtd-lf =
              cur-net-mep-comm (i0 1) - lme-net-mep-comm (i0 1)
           compute det-mtd-ah =
              cur-net-mep-comm (i0 2) - lme-net-mep-comm (i0 2)
           compute det-ytd-lf =
              cur-net-mep-comm (i0 1) - lye-net-mep-comm (i0 1)
           compute det-ytd-ah =
              cur-net-mep-comm (i0 2) - lye-net-mep-comm (i0 2)
           move cur-net-mep-comm (i0 1) to det-itd-lf
           move cur-net-mep-comm (i0 2) to det-itd-ah
           compute det-itd-tot =
              cur-net-mep-comm (i0 1) + cur-net-mep-comm (i0 2)
           MOVE '0'                    TO X
           MOVE detail-line            TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT


      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   They asked me to plug the itd mep commission due into    ***
      *** the mtd commission due and to compute the mtd prior mep    ***
      *** due as mep comm due + prior pmts - net mep comm.           ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

jeffw *    move '      Prior MEP Due           '
jeffw *                                to det-desc
jeffw *    move zeros                  to det-mtd-lf
jeffw *                                   det-mtd-ah
jeffw *                                   det-ytd-lf
jeffw *                                   det-ytd-ah
jeffw *                                   det-itd-lf
jeffw *                                   det-itd-ah
jeffw *    compute det-mtd-lf = cur-mep-due (i0 1) +
jeffw *       (cur-retro-pmts (i0 1) - lme-retro-pmts (i0 1)) -
jeffw *       (cur-net-mep-comm (i0 1) - lme-net-mep-comm (i0 1))
jeffw *    compute det-mtd-ah = cur-mep-due (i0 2) +
jeffw *       (cur-retro-pmts (i0 2) - lme-retro-pmts (i0 2)) -
jeffw *       (cur-net-mep-comm (i0 2) - lme-net-mep-comm (i0 2))
jeffw *
jeffw *    MOVE '0'                    TO X
jeffw *    MOVE detail-line            TO P-DATA
jeffw *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
      ***                                                            ***
      ***   Since the retro payments are entered at the account      ***
      *** level in Logic but actually paid at the group level we     ***
      *** don't want to report the payments at the account level.    ***
      ***                                                            ***
      ***_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

           if i0 = +1 or +2
              MOVE '-'                 TO X
              MOVE spaces              TO P-DATA
              PERFORM 0610-PRT-RTN     THRU 0610-EXIT
              move '0'                 to x
              move spaces              to p-data
              PERFORM 0610-PRT-RTN     THRU 0610-EXIT
              go to 4400-bypass-prior
           end-if

           move 'Less prior payments           '
      *    move '   Prior Retro Paid           '
                                       to mep-desc
jeffw *    compute mep-mtd-lf =
jeffw *       cur-retro-pmts (i0 1) - lme-retro-pmts (i0 1)
jeffw *    compute mep-mtd-ah =
jeffw *       cur-retro-pmts (i0 2) - lme-retro-pmts (i0 2)
jeffw      move zeros                  to mep-mtd-lf
jeffw                                     mep-mtd-ah
jeffw                                     mep-ytd-lf
jeffw                                     mep-ytd-ah
jeffw                                     mep-itd-lf
jeffw                                     mep-itd-ah
jeffw *    compute mep-ytd-lf =
jeffw *       cur-retro-pmts (i0 1) - lye-retro-pmts (i0 1)
jeffw *    compute mep-ytd-ah =
jeffw *       cur-retro-pmts (i0 2) - lye-retro-pmts (i0 2)
jeffw *    move cur-retro-pmts (i0 1)  to mep-itd-lf
jeffw *    move cur-retro-pmts (i0 2)  to mep-itd-ah
           compute mep-itd-tot =
              cur-retro-pmts (i0 1) + cur-retro-pmts (i0 2)
jeffw      MOVE '0'                    TO X
jeffw      MOVE mep-detail-line        TO P-DATA
jeffw      PERFORM 0610-PRT-RTN        THRU 0610-EXIT


      *    move '   System Generated           '
      *                                to det-misc-desc
      *    MOVE '0'                    TO X
      *    MOVE detail-desc-line       TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT
      *    move 'Retro Commission Due          '
      *                                to mep-desc

jeffw *    move ' MEP Commission Due           '
jeffw *                                to mep-desc
      *    compute mep-mtd-lf =
      *       cur-mep-due (i0 1) - lme-mep-due (i0 1)
      *    compute mep-mtd-ah =
      *       cur-mep-due (i0 2) - lme-mep-due (i0 2)
      *    compute mep-ytd-lf =
      *       cur-mep-due (i0 1) - lye-mep-due (i0 1)
      *    compute mep-ytd-ah =
      *       cur-mep-due (i0 2) - lye-mep-due (i0 2)

           move '0'                    to x
           move spaces                 to p-data
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT
           move ' '                    to x
           move '          Estimated'  to p-data

           PERFORM 0610-PRT-RTN        THRU 0610-EXIT
            
           move '   Cont. Commission'  to mep-desc
           move zeros                  to mep-ytd-lf
                                          mep-ytd-ah
                                          mep-mtd-lf
                                          mep-mtd-ah
                                          mep-itd-lf
                                          mep-itd-ah

      *    move cur-mep-due (i0 1)     to mep-itd-lf
      *    move cur-mep-due (i0 2)     to mep-itd-ah
           compute mep-itd-tot =
              cur-mep-due (i0 1) + cur-mep-due (i0 2)
           MOVE ' '                    TO X
           MOVE mep-detail-line        TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           .
       4400-bypass-prior.

           move 'Incurred Loss Ratio           '
                                       to ilr-desc

           compute ws-work-loss-ratio rounded =
              ((cur-clm-resv (i0 1) - lme-clm-resv (i0 1))
              + (cur-clm-pmts (i0 1) - lme-clm-pmts (i0 1)))
              / (cur-earn-prem (i0 1) - lme-earn-prem (i0 1)) * +100
           move ws-work-loss-ratio     to ilr-mtd-lf

           compute ws-work-loss-ratio rounded =
              ((cur-clm-resv (i0 2) - lme-clm-resv (i0 2))
              + (cur-clm-pmts (i0 2) - lme-clm-pmts (i0 2)))
              / (cur-earn-prem (i0 2) - lme-earn-prem (i0 2)) * +100
           move ws-work-loss-ratio     to ilr-mtd-ah

           compute ws-work-loss-ratio rounded =
              ((cur-clm-resv (i0 1) - lye-clm-resv (i0 1))
              + (cur-clm-pmts (i0 1) - lye-clm-pmts (i0 1)))
              / (cur-earn-prem (i0 1) - lye-earn-prem (i0 1)) * +100
           move ws-work-loss-ratio     to ilr-ytd-lf

           compute ws-work-loss-ratio rounded =
              ((cur-clm-resv (i0 2) - lye-clm-resv (i0 2))
              + (cur-clm-pmts (i0 2) - lye-clm-pmts (i0 2)))
              / (cur-earn-prem (i0 2) - lye-earn-prem (i0 2)) * +100
           move ws-work-loss-ratio     to ilr-ytd-ah

           compute ws-work-loss-ratio rounded =
              (cur-clm-resv (i0 1) + cur-clm-pmts (i0 1)) /
                 cur-earn-prem (i0 1) * +100
           move ws-work-loss-ratio     to ilr-itd-lf

           compute ws-work-loss-ratio rounded =
              (cur-clm-resv (i0 2) + cur-clm-pmts (i0 2)) /
                 cur-earn-prem (i0 2) * +100
           move ws-work-loss-ratio     to ilr-itd-ah

           compute ws-work-loss-ratio rounded = 
              (cur-clm-resv (i0 1) + cur-clm-resv (i0 2) +
              cur-clm-pmts (i0 1) + cur-clm-pmts (i0 2)) /
              (cur-earn-prem (i0 1) + cur-earn-prem (i0 2)) *  +100
           move ws-work-loss-ratio     to ilr-itd-tot

           MOVE '0'                    TO X
           MOVE spaces                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           MOVE '0'                    TO X
           MOVE ilr-detail-line        TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

jeffw *    move '      Beginning UEP           '
jeffw *                                to det-desc
jeffw *    compute det-mtd-lf = lme-uep (i0 1)
jeffw *    compute det-mtd-ah = lme-uep (i0 2)
jeffw *    compute det-ytd-lf = lye-uep (i0 1)
jeffw *    compute det-ytd-ah = lye-uep (i0 2)
jeffw *    move +0                     to det-itd-lf
jeffw *                                   det-itd-ah
jeffw *    MOVE '0'                    TO X
jeffw *    MOVE detail-line            TO P-DATA
jeffw *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT
jeffw *
jeffw *    move '         Ending UEP           '
jeffw *                                to det-desc
jeffw *    move cur-uep (i0 1)         to det-mtd-lf
jeffw *                                   det-ytd-lf
jeffw *                                   det-itd-lf
jeffw *    move cur-uep (i0 2)         to det-mtd-ah
jeffw *                                   det-ytd-ah
jeffw *                                   det-itd-ah
jeffw *    MOVE ' '                    TO X
jeffw *    MOVE detail-line            TO P-DATA
jeffw *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           .
       4400-continue.

           move sw-acct-key            to ws-prev-acct-key

           .
       4400-exit.
           exit.

       4500-accum-levels.

           perform varying i1 from +1 by +1 until i1 > +2
              
              compute cur-gross-prem (o9 i1) =
                 cur-gross-prem (o9 i1) + cur-gross-prem (i9 i1)
              compute cur-cancel-prem (o9 i1) =
                 cur-cancel-prem (o9 i1) + cur-cancel-prem (i9 i1)
              compute cur-net-prem (o9 i1) =
                 cur-net-prem (o9 i1) + cur-net-prem (i9 i1)
              compute cur-uep (o9 i1) =
                 cur-uep (o9 i1) + cur-uep (i9 i1)
              compute cur-earn-prem (o9 i1) =
                 cur-earn-prem (o9 i1) + cur-earn-prem (i9 i1)
              compute cur-inv-income (o9 i1) =
                 cur-inv-income (o9 i1) + cur-inv-income (i9 i1)
              compute cur-tot-income (o9 i1) =
                 cur-tot-income (o9 i1) + cur-tot-income (i9 i1)
              compute cur-acct-comms (o9 i1) =
                 cur-acct-comms (o9 i1) + cur-acct-comms (i9 i1)
              compute cur-ow-comms (o9 i1) =
                 cur-ow-comms (o9 i1) + cur-ow-comms (i9 i1)
              compute cur-ret-fees (o9 i1) =
                 cur-ret-fees (o9 i1) + cur-ret-fees (i9 i1)
              compute cur-clm-pmts (o9 i1) =
                 cur-clm-pmts (o9 i1) + cur-clm-pmts (i9 i1)
              compute cur-prem-tax (o9 i1) =
                 cur-prem-tax (o9 i1) + cur-prem-tax (i9 i1)
              compute cur-other-expenses (o9 i1) =
                 cur-other-expenses (o9 i1) + cur-other-expenses (i9 i1)
              compute cur-tot-expenses (o9 i1) =
                 cur-tot-expenses (o9 i1) + cur-tot-expenses (i9 i1)
              compute cur-net-cash-flow (o9 i1) =
                 cur-net-cash-flow (o9 i1) + cur-net-cash-flow (i9 i1)
              compute cur-mort-resv (o9 i1) =
                 cur-mort-resv (o9 i1) + cur-mort-resv (i9 i1)
              compute cur-clm-resv (o9 i1) =
                 cur-clm-resv (o9 i1) + cur-clm-resv (i9 i1)
              compute cur-net-mep-comm (o9 i1) =
                 cur-net-mep-comm (o9 i1) + cur-net-mep-comm (i9 i1)
              compute cur-retro-pmts (o9 i1) =
                 cur-retro-pmts (o9 i1) + cur-retro-pmts (i9 i1)
              compute cur-mep-due (o9 i1) =
                 cur-mep-due (o9 i1) + cur-mep-due (i9 i1)

              compute lme-gross-prem (o9 i1) =
                 lme-gross-prem (o9 i1) + lme-gross-prem (i9 i1)
              compute lme-cancel-prem (o9 i1) =
                 lme-cancel-prem (o9 i1) + lme-cancel-prem (i9 i1)
              compute lme-net-prem (o9 i1) =
                 lme-net-prem (o9 i1) + lme-net-prem (i9 i1)
              compute lme-uep (o9 i1) =
                 lme-uep (o9 i1) + lme-uep (i9 i1)
              compute lme-earn-prem (o9 i1) =
                 lme-earn-prem (o9 i1) + lme-earn-prem (i9 i1)
              compute lme-inv-income (o9 i1) =
                 lme-inv-income (o9 i1) + lme-inv-income (i9 i1)
              compute lme-tot-income (o9 i1) =
                 lme-tot-income (o9 i1) + lme-tot-income (i9 i1)
              compute lme-acct-comms (o9 i1) =
                 lme-acct-comms (o9 i1) + lme-acct-comms (i9 i1)
              compute lme-ow-comms (o9 i1) =
                 lme-ow-comms (o9 i1) + lme-ow-comms (i9 i1)
              compute lme-ret-fees (o9 i1) =
                 lme-ret-fees (o9 i1) + lme-ret-fees (i9 i1)
              compute lme-clm-pmts (o9 i1) =
                 lme-clm-pmts (o9 i1) + lme-clm-pmts (i9 i1)
              compute lme-prem-tax (o9 i1) =
                 lme-prem-tax (o9 i1) + lme-prem-tax (i9 i1)
              compute lme-other-expenses (o9 i1) =
                 lme-other-expenses (o9 i1) + lme-other-expenses (i9 i1)
              compute lme-tot-expenses (o9 i1) =
                 lme-tot-expenses (o9 i1) + lme-tot-expenses (i9 i1)
              compute lme-net-cash-flow (o9 i1) =
                 lme-net-cash-flow (o9 i1) + lme-net-cash-flow (i9 i1)
              compute lme-mort-resv (o9 i1) =
                 lme-mort-resv (o9 i1) + lme-mort-resv (i9 i1)
              compute lme-clm-resv (o9 i1) =
                 lme-clm-resv (o9 i1) + lme-clm-resv (i9 i1)
              compute lme-net-mep-comm (o9 i1) =
                 lme-net-mep-comm (o9 i1) + lme-net-mep-comm (i9 i1)
              compute lme-retro-pmts (o9 i1) =
                 lme-retro-pmts (o9 i1) + lme-retro-pmts (i9 i1)
              compute lme-mep-due (o9 i1) =
                 lme-mep-due (o9 i1) + lme-mep-due (i9 i1)

              compute lye-gross-prem (o9 i1) =
                 lye-gross-prem (o9 i1) + lye-gross-prem (i9 i1)
              compute lye-cancel-prem (o9 i1) =
                 lye-cancel-prem (o9 i1) + lye-cancel-prem (i9 i1)
              compute lye-net-prem (o9 i1) =
                 lye-net-prem (o9 i1) + lye-net-prem (i9 i1)
              compute lye-uep (o9 i1) =
                 lye-uep (o9 i1) + lye-uep (i9 i1)
              compute lye-earn-prem (o9 i1) =
                 lye-earn-prem (o9 i1) + lye-earn-prem (i9 i1)
              compute lye-inv-income (o9 i1) =
                 lye-inv-income (o9 i1) + lye-inv-income (i9 i1)
              compute lye-tot-income (o9 i1) =
                 lye-tot-income (o9 i1) + lye-tot-income (i9 i1)
              compute lye-acct-comms (o9 i1) =
                 lye-acct-comms (o9 i1) + lye-acct-comms (i9 i1)
              compute lye-ow-comms (o9 i1) =
                 lye-ow-comms (o9 i1) + lye-ow-comms (i9 i1)
              compute lye-ret-fees (o9 i1) =
                 lye-ret-fees (o9 i1) + lye-ret-fees (i9 i1)
              compute lye-clm-pmts (o9 i1) =
                 lye-clm-pmts (o9 i1) + lye-clm-pmts (i9 i1)
              compute lye-prem-tax (o9 i1) =
                 lye-prem-tax (o9 i1) + lye-prem-tax (i9 i1)
              compute lye-other-expenses (o9 i1) =
                 lye-other-expenses (o9 i1) + lye-other-expenses (i9 i1)
              compute lye-tot-expenses (o9 i1) =
                 lye-tot-expenses (o9 i1) + lye-tot-expenses (i9 i1)
              compute lye-net-cash-flow (o9 i1) =
                 lye-net-cash-flow (o9 i1) + lye-net-cash-flow (i9 i1)
              compute lye-mort-resv (o9 i1) =
                 lye-mort-resv (o9 i1) + lye-mort-resv (i9 i1)
              compute lye-clm-resv (o9 i1) =
                 lye-clm-resv (o9 i1) + lye-clm-resv (i9 i1)
              compute lye-net-mep-comm (o9 i1) =
                 lye-net-mep-comm (o9 i1) + lye-net-mep-comm (i9 i1)
              compute lye-retro-pmts (o9 i1) =
                 lye-retro-pmts (o9 i1) + lye-retro-pmts (i9 i1)
              compute lye-mep-due (o9 i1) =
                 lye-mep-due (o9 i1) + lye-mep-due (i9 i1)
           end-perform

           .
       4500-exit.
           exit.

       0590-HEADING-ROUTINE.

           MOVE '1'                    TO X
           MOVE HEAD-1                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           MOVE ' '                    TO X
           MOVE HEAD-2                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           ADD +1                      TO PAGE-CNT
           MOVE PAGE-CNT               TO H3-PAGE
           MOVE HEAD-3                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           MOVE '0'                    TO X
           MOVE HEAD-4                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           MOVE ' '                    TO X
           MOVE HEAD-5                 TO P-DATA
           PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           move '-'                    to x
           move spaces                 to p-data
           perform 0610-prt-rtn        thru 0610-exit

           move '-'                    to x
           move spaces                 to p-data
           perform 0610-prt-rtn        thru 0610-exit

           move '-'                    to x
           move spaces                 to p-data
           perform 0610-prt-rtn        thru 0610-exit

      *    MOVE ' '                    TO X
      *    MOVE HEAD-6                 TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      *    MOVE ' '                    TO X
      *    MOVE HEAD-7                 TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

      *    MOVE ' '                    TO X
      *    MOVE HEAD-8                 TO P-DATA
      *    PERFORM 0610-PRT-RTN        THRU 0610-EXIT

           .
       0590-EXIT.                                                       
           EXIT.                                                        

       0610-PRT-RTN.                                                    

                                       COPY ELCPRT2.
           .
       0610-EXIT.                                                       
           EXIT.                                                        

       1000-connect.

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate


           move 'CSO_Retro'            to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

      *     EXEC SQL
NTTDel**       CONNECT TO :svr USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
              perform abend-pgm
           end-if

           display " connect to DB successful "

           .
       1000-exit.
           exit.

       1010-declare-cursor.

           display ' declare cursor '

           EXEC SQL
              declare retrostatus cursor for
              select distinct PoolCode, Status, ReportCode3, RetroName,
                 EffectiveDate, NextReportMonth
               from Retro
                 where Status = 3 or Status = 4 or Status = 5
                    or Status = 10 order by PoolCode
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 1050-disconnect  thru 1050-exit
              perform abend-pgm
           end-if

           .
       1010-exit.
           exit.

       1020-open-cursor.

           display ' open cursor '

           EXEC SQL
              open retrostatus
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform 1050-disconnect  thru 1050-exit
              perform abend-pgm
           end-if

           .
       1020-exit.
           exit.

       1030-process-input.

           move high-values            to ws-retro-group-table
           move +0                     to i1
           perform until sqlcode not = 0
              EXEC SQL
                 fetch retrostatus into :tb-Retro-Group,
                                        :tb-Retro-Status,
                                        :tb-report-code-3,
                                        :tb-retro-name,
                                        :tb-retro-eff-dte,
                                        :tb-retro-next-rpt-mo
              END-EXEC
              if sqlcode not = 0 and 100
                 display "Error: cannot fetch row "
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 perform 1050-disconnect thru 1050-exit
                 perform abend-pgm
              end-if
              if sqlcode = 0
                 add +1                to i1
                 move tb-retro-group   to ws-retro-group (i1)
                 move tb-retro-status  to ws-status (i1)
                 move tb-report-code-3 to ws-report-code-3 (i1)
                 move tb-retro-name    to ws-retro-name (i1)
                 move tb-retro-next-rpt-mo
                                       to ws-retro-next-rpt-mo (i1)
                 display ' retro eff date ' tb-retro-eff-dte
                 string tb-retro-eff-dte (6:2) '/'
                        tb-retro-eff-dte (9:2) '/'
                        tb-retro-eff-dte (1:4) delimited by size
                        into ws-retro-eff-dte (i1)
                 end-string
                 add 1                 to ws-recs-in
              end-if
           end-perform

           if sqlcode = 100
              display ' Normal end of record set '
              display ' number of records        ' ws-recs-in
           end-if

           .
       1030-exit.
           exit.

       1050-disconnect.

           display ' Begin Disconnect '
           EXEC SQL
              disconnect
           END-EXEC
           if sqlcode not = 0
              display "Error: disconnect "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           .
       1050-exit.
           exit.

       5000-close-files.

           close   eracct ERPLAN-IN EPEC-FILE PRNTR index-file

           .
       5000-exit.
           exit.

       8500-date-convert.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   

       8500-exit.
           EXIT.                                                        

02918  ABEND-PGM SECTION.                                               
02919                              COPY ELCABEND.
