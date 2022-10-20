      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  ID DIVISION.                                                     
00002                                                                   
00003  PROGRAM-ID.                 EL677.                               
00008 *                                                                 
00009 *AUTHOR.     CSO
00010 *            OMAHA, NEBRASKA.                                     
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.    TRANSACTION - EXF3 - CHECK MAINTENANCE               

111513******************************************************************
111513*                   C H A N G E   L O G
111513*
111513* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111513*-----------------------------------------------------------------
111513*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111513* EFFECTIVE    NUMBER
111513*-----------------------------------------------------------------
111513* 111513  CR2013053000001  PEMA  DAILY CHECK REQUEST CHANGES
021714* 021714  CR2014021700001  PEMA  ADD TEST DB FOR OTHER THAN cid1p
030414* 030414  IR2014030400001  PEMA  CHG PYAJ VOID TRANS TO R AND +
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
091615* 091615  CR2015082000001  PEMA  ADD TOTAL ENDT PROCESSING
020317* 020317  IR2016110900001  PEMA  FIXED MISC PROBLEMS
060717* 060717  CR2017032900002  PEMA  CHANGE TEST DB
111418* 111418  CR2018103100001  PEMA  ADD REISSUE CAPABILITY
040919* 040919  CR2019040900001  PEMA  FIX RETURN TO ON REISSUES
111219* 111219  CR2019110700001  PEMA  ALLOW REVERSAL OF VOID ON SAME DAY
030921* 030921  CR2019012500003  PEMA  Connect to sdv-db01.cso.local
111513******************************************************************

00026  ENVIRONMENT DIVISION.                                            
00027                                                                   
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         

00031  77  FILLER  PIC X(32)  VALUE '********************************'. 
00032  77  FILLER  PIC X(32)  VALUE '*    EL677 WORKING STORAGE     *'. 
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 

       77  s1                          pic s999 comp-3 value +0.
       77  ws-tot-lf-prem              pic s9(7)v99 comp-3 value +0.
       77  ws-tot-iss-prem             pic s9(7)v99 comp-3 value +0.
       77  ws-tot-iss-comm             pic s9(7)v99 comp-3 value +0.
       77  ws-tot-ref-comm             pic s9(7)v99 comp-3 value +0.
111219 77  ws-pyaj-browse-sw           pic x value spaces.
111219     88  pyaj-browse-started        value 'Y'.
       77  ws-browse-sw                pic x value spaces.
           88  i-say-when                 value 'Y'.
       77  ws-delete-sw                pic x value ' '.
           88  row-deleted                 value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-match-sw                 pic x  value ' '.
           88  found-a-match             value 'Y'.
       77  ws-commit-sw                pic x value ' '.
           88  tbl-commited                value 'Y'.

021714 01  P pointer.
021714 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
021714 01  var-ptr pointer.
021714 01  env-var-len                 pic 9(4)  binary.
021714
021714 01  WS-KIXSYS.
021714     05  WS-KIX-FIL1             PIC X(10).
021714     05  WS-KIX-APPS             PIC X(10).
021714     05  WS-KIX-ENV              PIC X(10).
021714     05  WS-KIX-MYENV            PIC X(10).
021714     05  WS-KIX-SYS              PIC X(10).

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  ws-paid-bank-work-area.
           05  ws-pb-compid            pic x(9).
           05  ws-pb-check-no          pic x(10).
           05  ws-pb-bad-check-no      pic x(10).
021714     05  ws-check-amount         pic x(10).

       01  Paid-Bank-Info.
           05  pb-check-no             pic 9(10).
           05  pb-tran-type            pic x.
           05  pb-bank-acct-desc       pic x(50).
           05  pb-amount               pic x(12).
           05  pb-paid-date            pic x(25).

       01  ws-key-stuff.
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic x.
091615     05  ws-check-sub-type       pic x.

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.

       01  daily-check-request-rec.
           05  db-compid               pic xxx.
           05  db-carrier              pic x.
           05  db-grouping             pic x(6).
           05  db-state                pic xx.
           05  db-account              pic x(10).
           05  db-effdate              pic x(10).
           05  db-certificate          pic x(10).
           05  db-cert-sfx             pic x.
           05  db-seq-no               pic 999.
           05  db-type                 pic x.
           05  db-amount-n             pic 9(7).99.
           05  db-amount redefines db-amount-n
                                       pic x(10).
           05  db-checkno              pic x(15).
           05  db-checkdate            pic x(10).
           05  db-checkstatus          pic 9(5).
           05  db-releasebatch         pic 9(5).
           05  db-releasedt            pic x(10).
           05  db-releaseby            pic x(4).
           05  db-payeename1           pic x(30).
           05  db-payeename2           pic x(30).
           05  db-payeeaddr1           pic x(30).
           05  db-payeeaddr2           pic x(30).
           05  db-payeecity            pic x(30).
           05  db-payeest              pic xx.
           05  db-payeezip             pic x(10).
           05  db-fincar               pic x.
           05  db-fingrp               pic x(6).
           05  db-finresp              pic x(10).
           05  db-finacct              pic x(10).
           05  db-preparer             pic x(4).
           05  db-app-status           pic x(9).
           05  dp-app-status-n redefines db-app-status
                                       pic 9(9).
           05  db-app-by               pic x(20).
           05  db-app-date             pic x(30).
           05  db-app-batch            pic x(10).
           05  db-return-to            pic x(30).
           05  db-insured-name         pic x(30).
091615     05  db-check-sub-type       pic x.

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

00035                              COPY ELCSCTM.                        
00036                              COPY ELCSCRTY.                       
       01  ws-dummy-erchek             pic x(600).
       01  filler.
021714     05  WS-CHECK-AMT-TMP        PIC Z(7).99.
021714     05  WS-CHECK-AMT-TMPX REDEFINES             
021714         WS-CHECK-AMT-TMP        PIC X(10).  
       01  ws-compare-erchek-key          pic x(33) value low-values.
       01  ws-eracct-sw                   pic x value ' '.
           88  eracct-found                 value 'Y'.
       01  ws-eracct-start-sw             pic x value ' '.
           88  eracct-start                 value 'Y'.
00040  01  WS-MISC-AREA.
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    
00042      05  SAVE-CURRENT-DATE-MDY       PIC X(6)    VALUE SPACES.    
00043      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    
00044                                                                   
       01  ws-user-defined.
           05  ws-user-reason         pic xxx.
           05  ws-user-cert-no        pic x(11).
           05  ws-user-name           pic x(28).

       01  ws-hold-eracct-record       pic x(2000)  value spaces.
00045  01  STANDARD-AREAS.                                              
           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
00046      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.     
00047      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     
00048      12  EL677A                      PIC X(8)    VALUE 'EL677A'.  
00049      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL677S'.  
00050      12  SCREEN-NUMBER               PIC X(4)    VALUE '677A'.    
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXF3'.    
00052      12  EL6311-TRANS-ID             PIC X(4)    VALUE 'EXB1'.    
091615     12  EL6318-TRANS-ID             PIC X(4)    VALUE 'EXBE'.    
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL677'.   
00055      12  PGM-NAME                    PIC X(8)   VALUE SPACES.     
00056      12  TIME-IN                     PIC S9(7)  VALUE ZEROS.      
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00058          16  FILLER                  PIC X.                       
00059          16  TIME-OUT                PIC 99V99.                   
00060          16  FILLER                  PIC XX.                      
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   
00063      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   
00064      12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.  
00065      12  XCTL-114                    PIC X(8)    VALUE 'EL114'.   
00066      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   
00067      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   
00068      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. 
00069      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.  
00071      12  ERACCT-FILE-ID              PIC X(8)    VALUE 'ERACCT'.  
00072      12  ERCOMP-FILE-ID              PIC X(8)    VALUE 'ERCOMP'.  
00073      12  ERCHEK-FILE-ID              PIC X(8)    VALUE 'ERCHEK'.  
00075      12  ERPNDB-FILE-ID              PIC X(8)    VALUE 'ERPNDB'.  
00076      12  ERPNDB-ALT-FILE-ID          PIC X(8)    VALUE 'ERPNDB2'. 
00077      12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.  
00078      12  ERMAIL-FILE-ID              PIC X(8)    VALUE 'ERMAIL'.  
00081      12  ERPYAJ-FILE-ID              PIC X(8)    VALUE 'ERPYAJ'.  
00082      12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +0    COMP.
00084      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.    
00085      12  QID.                                                     
00086          16  QID-TERM                PIC X(4)    VALUE SPACES.    
00087          16  FILLER                  PIC X(4)    VALUE '677A'.    
00088                                                                   
00089  01  WORK-AREAS.                                                  
00090      12  WS-AM-WORK                  PIC S9(8)V99 VALUE +0.       
00091      12  WS-AM-WK REDEFINES WS-AM-WORK.                           
00092          16  WS-AM-NUM               PIC S9(7)V99.                
00093          16  WS-AM-SIGN              PIC X.                       
00094      12  WS-CHECK-WORK.                                           
00095          16  FILLER                  PIC X        VALUE SPACE.    
00096          16  WS-CHECK-NO             PIC X(6)     VALUE SPACES.   
00097      12  WS-CHK-PRINT-DT             PIC XX       VALUE LOW-VALUE.
00098      12  WS-SV-AMOUNT-PAID           PIC S9(7)V99 VALUE +0.       
00099      12  WS-AMT                      PIC S9(7)V99 VALUE +0.       
00103      12  WS-REFUND-AMOUNT            PIC S9(7)V99 VALUE +0.       
00107      12  WS-DEEDIT-FIELD             PIC S9(9)V99 VALUE +0.       
00108      12  WS-DT-DEEDIT-FIELD          PIC X(10)   VALUE ZERO.      
00109      12  WS-DEEDIT-FIELD-DATE        REDEFINES                    
00110          WS-DT-DEEDIT-FIELD.                                      
00111          16  FILLER                  PIC X(4).                    
00112          16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).                   
CIDMOD     12  WS-DEEDIT-FIELD-A           PIC X(15)   VALUE ZERO.      
CIDMOD     12  WS-DEEDIT-FIELD-V0          REDEFINES                    
CIDMOD         WS-DEEDIT-FIELD-A           PIC S9(15).                  
00113                                                                   
00114      12  WS-SUB                      PIC S9(4)   VALUE ZERO  COMP.
00117      12  WS-CERT-FOUND-SW            PIC X       VALUE 'N'.       
00118          88 WS-CERT-FOUND                        VALUE 'Y'.       
00119          88 WS-CERT-NOT-FOUND                    VALUE 'N'.       
00120      12  WS-PNDB-FOUND-SW            PIC X       VALUE 'N'.       
00121          88 WS-PNDB-FOUND                        VALUE 'Y'.       
00122          88 WS-PNDB-NOT-FOUND                    VALUE 'N'.       
00123      12  WS-PROCESS-CERT-SW          PIC X       VALUE SPACE.     
00124          88  WS-PROCESS-CERT                     VALUE 'Y'.       
00125      12  WS-PROCESS-BENEFICIARY-SW   PIC X       VALUE SPACE.     
00126          88  WS-PROCESS-BENEFICIARY              VALUE 'Y'.       
00163      12  WS-TMS-ENTRY-COMMENT.                                    
00164          16  WS-TMS-PY-CERT          PIC X(10)   VALUE SPACES.    
00165          16  FILLER                  PIC XX      VALUE SPACES.    
00166          16  WS-TMS-PY-PAYEE         PIC X(18)   VALUE SPACES.    
00167      12  WS-ZIP-CODE.                                             
00168          16  WS-ZIP-PRIME.                                        
00169              20  WS-ZIP-PRI-1ST      PIC X.                       
00170                  88  WS-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'. 
00171              20  FILLER              PIC X(4).                    
00172          16  WS-ZIP-PLUS4            PIC X(4).                    
00173      12  WS-CANADIAN-POST-CODE1  REDEFINES  WS-ZIP-CODE.          
00174          16  WS-CAN-POST1-1          PIC XXX.                     
00175          16  WS-CAN-POST1-2.                                      
00176              20  WS-CAN-POST-4TH     PIC X.                       
00177              20  FILLER              PIC XX.                      
00178          16  FILLER                  PIC XXX.                     
00179      12  WS-CANADIAN-POST-CODE2  REDEFINES  WS-ZIP-CODE.          
00180          16  WS-CAN-POST2-1          PIC XXX.                     
00181          16  FILLER                  PIC X.                       
00182          16  WS-CAN-POST2-2          PIC XXX.                     
00183          16  FILLER                  PIC XX.                      
00184                                                                   
00185                                      COPY ELCNWA.                 

       01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL                    VALUE +0.
           88  resp-file-notfnd               value +12.
           88  RESP-NOTFND                    VALUE +13.
           88  resp-duprec                    value +14.
           88  resp-dupkey                    value +15.
           88  resp-invreq                    value +16.
           88  RESP-NOTOPEN                   VALUE +19.
           88  RESP-ENDFILE                   VALUE +20.
           88  resp-lengtherr                 value +22.

00188  01  ACCESS-KEYS.                                                 
00194      12  ELCNTL-KEY.                                              
00195          16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.    
00196          16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.    
00197          16 ELCNTL-ACCESS.                                        
00198              20 ELCNTL-STATE         PIC XX      VALUE SPACES.    
00199              20  FILLER              PIC X       VALUE SPACES.    
00200              20 ELCNTL-CARRIER       PIC X       VALUE SPACES.    
00201          16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.
00202                                                                   
00205      12  ERACCT-KEY.                                              
00206          16  ERACCT-CO               PIC X       VALUE SPACES.    
00207          16  ERACCT-CARRIER          PIC X       VALUE SPACES.    
00208          16  ERACCT-GROUPING         PIC X(6)    VALUE SPACES.    
00209          16  ERACCT-STATE            PIC XX      VALUE SPACES.    
00210          16  ERACCT-ACCOUNT          PIC X(10)   VALUE SPACES.    
00211          16  ERACCT-EXP-DATE         PIC XX      VALUE SPACES.    
00212          16  ERACCT-EXP-DATE-FILLER  PIC X(4)    VALUE SPACES.    
00213                                                                   
00214      12  SAVE-ERACCT-KEY.                                         
00215          16  SV-ACCT-CO              PIC X       VALUE SPACES.    
00216          16  SV-ACCT-CARRIER         PIC X       VALUE SPACES.    
00217          16  SV-ACCT-GROUPING        PIC X(6)    VALUE SPACES.    
00218          16  SV-ACCT-STATE           PIC XX      VALUE SPACES.    
00219          16  SV-ACCT-ACCOUNT         PIC X(10)   VALUE SPACES.    
00220          16  SV-ACCT-EXP-DATE        PIC XX      VALUE SPACES.    
00221          16  SV-ACCT-EXP-DATE-FILLER PIC X(4)    VALUE SPACES.    
00222                                                                   
00225      12  ERCOMP-KEY.                                              
00226          16  ERCOMP-COMP-CD          PIC X       VALUE SPACE.     
00227          16  ERCOMP-CARRIER          PIC X       VALUE SPACES.    
00228          16  ERCOMP-GROUPING         PIC X(6)    VALUE SPACES.    
00229          16  ERCOMP-FIN-RESP         PIC X(10)   VALUE SPACES.    
00230          16  ERCOMP-ACCOUNT          PIC X(10)   VALUE SPACES.    
00231          16  ERCOMP-RECORD-TYPE      PIC X       VALUE SPACES.    
00232                                                                   
00235      12  ELCERT-KEY.                                              
00236          16 ELCERT-COMPANY-CD        PIC X       VALUE SPACES.    
00237          16 ELCERT-CARRIER           PIC X       VALUE SPACES.    
00238          16 ELCERT-GROUPING          PIC X(6)    VALUE SPACES.    
00239          16 ELCERT-STATE             PIC XX      VALUE SPACES.    
00240          16 ELCERT-ACCOUNT           PIC X(10)   VALUE SPACES.    
00241          16 ELCERT-CERT-EFF-DT       PIC XX      VALUE SPACES.    
00242          16 ELCERT-CERT-PRIME        PIC X(10)   VALUE SPACES.    
00243          16 ELCERT-CERT-SFX          PIC X       VALUE SPACES.    
00244                                                                   
00247      12  ERPNDB-PRIMARY-KEY.                                      
00248          16 ERPNDB-COMPANY-CD        PIC X       VALUE SPACES.    
00249          16 ERPNDB-ENTRY-BATCH       PIC X(6)    VALUE SPACES.    
00250          16 ERPNDB-BATCH-SEQ-NO      PIC S9(4)   VALUE +0    COMP.
00251          16 ERPNDB-BATCH-CHG-SEQ-NO  PIC S9(4)   VALUE +0    COMP.
00252                                                                   
00253      12  ERPNDB-ALT-KEY.                                          
00254          16 ERPNDB-ALT-COMPANY-CD    PIC X       VALUE SPACES.    
00255          16 ERPNDB-ALT-CARRIER       PIC X       VALUE SPACES.    
00256          16 ERPNDB-ALT-GROUPING      PIC X(6)    VALUE SPACES.    
00257          16 ERPNDB-ALT-STATE         PIC XX      VALUE SPACES.    
00258          16 ERPNDB-ALT-ACCOUNT       PIC X(10)   VALUE SPACES.    
00259          16 ERPNDB-ALT-CERT-EFF-DT   PIC XX      VALUE SPACES.    
00260          16 ERPNDB-ALT-CERT-PRIME    PIC X(10)   VALUE SPACES.    
00261          16 ERPNDB-ALT-CERT-SFX      PIC X       VALUE SPACES.    
00262          16 ERPNDB-ALT-CH-SEQ-NO     PIC S9(4)   VALUE +0    COMP.
00263          16 ERPNDB-ALT-RECORD-TYPE   PIC X       VALUE SPACE.     
00264                                                                   
00275      12  ERCHEK-KEY.                                              
00276          16  CHEK-COMPANY-CD         PIC X       VALUE SPACE.     
00277          16  CHEK-CARRIER            PIC X       VALUE SPACE.     
00278          16  CHEK-GROUPING           PIC X(6)    VALUE SPACES.    
00279          16  CHEK-STATE              PIC XX      VALUE SPACES.    
00280          16  CHEK-ACCOUNT            PIC X(10)   VALUE SPACES.    
00281          16  CHEK-EFF-DT             PIC XX      VALUE SPACES.    
00282          16  CHEK-CERT-NO            PIC X(10)   VALUE SPACES.    
00283          16  CHEK-SUF-NO             PIC X       VALUE SPACES.    
00284          16  CHEK-RECORD-SEQ         PIC S9(4)   VALUE ZEROS COMP.
00285                                                                   
00286      12  ERCHEK-RECORD-LENGTH        PIC S9(4)   VALUE +600  COMP.

00301      12  ERPYAJ-KEY.                                              
00302          16 PYAJ-COMPANY-CD          PIC X       VALUE LOW-VALUES.
00303          16 PYAJ-CARRIER             PIC X       VALUE SPACE.     
00304          16 PYAJ-GROUPING            PIC X(06)   VALUE SPACES.    
00305          16 PYAJ-FIN-RESP            PIC X(10)   VALUE SPACES.    
00306          16 PYAJ-ACCOUNT             PIC X(10)   VALUE SPACES.    
00307          16 PYAJ-FILE-SEQ-NO         PIC S9(8)   VALUE +0  COMP.  
00308          16 PYAJ-RECORD-TYPE         PIC X       VALUE SPACE.     
00309                                                                   
00313  01  ERROR-NUMBERS.                                               
00314      12  ER-0000                 PIC X(4)    VALUE '0000'.    
00315      12  ER-0004                 PIC X(4)    VALUE '0004'.    
00317      12  ER-0013                 PIC X(4)    VALUE '0013'.    
00318      12  ER-0022                 PIC X(4)    VALUE '0022'.    
00319      12  ER-0023                 PIC X(4)    VALUE '0023'.    
00321      12  ER-0029                 PIC X(4)    VALUE '0029'.    
00322      12  ER-0070                 PIC X(4)    VALUE '0070'.    
00323      12  ER-0194                 PIC X(4)    VALUE '0194'.    
00324      12  ER-0195                 PIC X(4)    VALUE '0195'.    
00325      12  ER-0196                 PIC X(4)    VALUE '0196'.    
00326      12  ER-0197                 PIC X(4)    VALUE '0197'.    
00327      12  ER-0203                 PIC X(4)    VALUE '0203'.    
00328      12  ER-0215                 PIC X(4)    VALUE '0215'.    
00329      12  ER-0216                 PIC X(4)    VALUE '0216'.    
00330      12  ER-0433                 PIC X(4)    VALUE '0433'.    
00332      12  ER-1159                 PIC X(4)    VALUE '1159'.    
00333      12  ER-1162                 PIC X(4)    VALUE '1162'.    
00334      12  ER-2056                 PIC X(4)    VALUE '2056'.    
00335      12  ER-2208                 PIC X(4)    VALUE '2208'.    
00336      12  ER-2209                 PIC X(4)    VALUE '2209'.    
00337      12  ER-2210                 PIC X(4)    VALUE '2210'.    
00338      12  ER-2230                 PIC X(4)    VALUE '2230'.    
00339      12  ER-2237                 PIC X(4)    VALUE '2237'.    
00340      12  ER-2238                 PIC X(4)    VALUE '2238'.    
00342      12  ER-2394                 PIC X(4)    VALUE '2394'.    
00345      12  ER-2583                 PIC X(4)    VALUE '2583'.    
00346      12  ER-2600                 PIC X(4)    VALUE '2600'.    
00347      12  ER-2726                 PIC X(4)    VALUE '2726'.    
00350      12  ER-2907                 PIC X(4)    VALUE '2907'.    
00351      12  ER-2908                 PIC X(4)    VALUE '2908'.    
00353      12  ER-3044                 PIC X(4)    VALUE '3044'.    
00354      12  ER-3046                 PIC X(4)    VALUE '3046'.    
111418     12  er-3274                 pic x(4)    value '3274'.
           12  er-3450                 pic x(4)    value '3450'.
           12  er-3451                 pic x(4)    value '3451'.
           12  er-3452                 pic x(4)    value '3452'.
           12  er-3453                 pic x(4)    value '3453'.
           12  er-3454                 pic x(4)    value '3454'.
           12  er-3455                 pic x(4)    value '3455'.
091615     12  er-3459                 pic x(4)    value '3459'.
111418     12  er-3460                 pic x(4)    value '3460'.
111219     12  er-3461                 pic x(4)    value '3461'.
111219     12  er-3462                 pic x(4)    value '3462'.
111219     12  er-3463                 pic x(4)    value '3463'.
           12  er-9999                 pic x(4)    value '9999'.

00360                                  COPY ELCLOGOF.               
00362                                  COPY ELCDATE.                
00364                                  COPY ELCATTR.                
00366                                  COPY ELCEMIB.                
00368                                  COPY ELCINTF.
00369      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
00370          16  PI-EOF-SW               PIC X.                       
00371              88  PI-FILE-EOF                     VALUE 'Y'.       
00372          16  PI-PREV-MAINT           PIC X.                       
00373          16  PI-WORK-SEQ-NO          PIC S9(9)   COMP-3.          
00374          16  PI-ERCHEK-KEY.                                       
00375              20  PI-CHEK-COMP-CD     PIC X.                       
00376              20  PI-CHEK-CARRIER     PIC X.                       
00377              20  PI-CHEK-GROUPING    PIC X(6).                    
00378              20  PI-CHEK-STATE       PIC XX.                      
00379              20  PI-CHEK-ACCOUNT     PIC X(10).                   
00380              20  PI-CHEK-EFF-DT      PIC XX.                      
00381              20  PI-CHEK-CERT-NO     PIC X(10).                   
00382              20  PI-CHEK-SUF-NO      PIC X.                       
00383              20  PI-CHEK-SEQUENCE    PIC S9(4)  COMP.             
00384          16  PI-ACCOUNT-ADDRESS.                                  
00385              20  PI-AM-NAME          PIC X(30).                   
00386              20  PI-AM-ADDRS         PIC X(30).                   
00387              20  PI-AM-CITY-st.
                       24  pi-am-city      PIC X(28).
                       24  pi-am-st        pic xx.
00388              20  PI-AM-ZIP-CODE.                                  
00389                  24  PI-AM-ZIP-PRIME PIC X(5).                    
00390                  24  PI-AM-ZIP-PLUS4 PIC X(4).                    
00391          16  PI-PROCESS-CERT-SW          PIC X.                   
00392              88  PI-PROCESS-CERT                     VALUE 'Y'.   
00393          16  PI-PROCESS-BENEFICIARY-SW   PIC X.                   
00394              88  PI-PROCESS-BENEFICIARY              VALUE 'Y'.   
00395          16  PI-PAYTO1                   PIC X(30).               
00396          16  PI-REFERENCE                PIC X(12).               
00397          16  PI-CANC-DT                  PIC XX.                  
00398          16  PI-LF-REFUND                PIC S9(7)V99  COMP-3.    
00399          16  PI-AH-REFUND                PIC S9(7)V99  COMP-3.    
00400          16  PI-INSURED-NAME             PIC X(28).               
00401          16  PI-PFKEY                    PIC XXX.                 
00402              88  PI-TO-EL1273-FROM-EL677        VALUE 'PF3'.      
00403              88  PI-TO-EL677-FROM-EL1273        VALUE 'PF8'.      
00404          16  PI-AMOUNT                   PIC S9(9)V99  COMP-3.    
00405          16  PI-TYPE                     PIC X.                   
               16  pi-prev-paid                pic s9(7)v99 comp-3.
               16  pi-refund-on-pending-rec    pic s9(7)v99 comp-3.
               16  pi-ue-comm                  pic s9(7)v99 comp-3.
               16  pi-chek-rec-cnt             pic s999 comp-3.
               16  pi-am-csr                   pic x(4).
               16  pi-return-to                pic x(30).
               16  pi-prev-ded-comm            pic x.
               16  pi-table-name               pic x(30).
030414         16  pi-check-cashed             pic x.
091615         16  pi-endt-prm-diff            pic s9(7)v99 comp-3.
091615         16  pi-endt-com-diff            pic s9(5)v99 comp-3.
091615         16  pi-check-type               pic x.
091615             88  pi-corr-check              value 'C'.
091615             88  pi-ref-check               value 'R'.
091615         16  pi-check-cut                pic x.
103116         16  pi-prev-paid-this-month     pic s9(7)v99 comp-3.
               16  pi-previous-deduct-comm     pic x.
111418         16  pi-void-reissue-pass        pic x.
111418             88  pi-void-complete           value '1'.
111418             88  pi-address-selected        value '2'.
111418         16  pi-void-reissue-amt         pic s9(7)v99 comp-3.
111418         16  FILLER                      PIC X(304). *> wass 311

00409                                      COPY ELCAID.                 
00410  01  FILLER    REDEFINES DFHAID.                                  
00411      12  FILLER                      PIC X(8).                    
00412      12  PF-VALUES                   PIC X       OCCURS 24.       
00413      12  FILLER                      PIC X(3).                    

00415                                      COPY EL677S.                 

00417  LINKAGE SECTION.                                                 
00418  01  DFHCOMMAREA.
091615     05  filler                  PIC X(1024).

021714 01  var  pic x(30).


00421                                  COPY ELCCNTL.
00423                                  COPY ERCCHEK.
00427                                  COPY ERCACCT.
00429                                  COPY ERCCOMP.
00431                                  COPY ELCCERT.
00433                                  COPY ERCPNDB.
00435                                  COPY ERCPNDM.
00437                                  COPY ERCMAIL.
00441                                  COPY ERCPYAJ.

00446  PROCEDURE DIVISION.                                              
00447                                                                   
00448      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
00449      MOVE '5'                    TO  DC-OPTION-CODE.              
00450      PERFORM 8500-DATE-CONVERT.                                   
00451      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   
00452      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               
00453      MOVE DC-GREG-DATE-1-MDY     TO  SAVE-CURRENT-DATE-MDY.       
00454                                                                   
00455      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     
00456                                                                   
00457      IF PI-AMOUNT NOT NUMERIC                                     
00458          MOVE ZEROS              TO  PI-AMOUNT.                   
00459                                                                   
00460      IF EIBCALEN = 0                                              
00461          GO TO 8800-UNAUTHORIZED-ACCESS.                          

021714     set P to address of KIXSYS
021714     CALL "getenv" using by value P returning var-ptr
021714     if var-ptr = null then
021714        display ' kixsys not set '
021714     else
021714        set address of var to var-ptr
021714        move 0 to env-var-len
021714        inspect var tallying env-var-len
021714          for characters before X'00' 
021714        unstring var (1:env-var-len) delimited by '/'
021714           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
021714              WS-KIX-SYS
021714        end-unstring
021714     end-if

00463      MOVE 2                      TO  EMI-NUMBER-OF-LINES.         
00464      MOVE 2                      TO  EMI-SWITCH2.                 
00465      MOVE EIBTRMID               TO  QID-TERM.                    
00466                                                                   
00467      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           
00468          MOVE PI-CALLING-PROGRAM TO  RETURNED-FROM.               
00469                                                                   
00470      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00471          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00472              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     
00473              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     
00474              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     
00475              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     
00476              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     
00477              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     
00478              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   
00479              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     
00480          ELSE                                                     
00481              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     
00482              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   
00483              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     
00484              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     
00485              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     
00486              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     
00487              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     
00488              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.    
00489                                                                   
00490      MOVE LOW-VALUES             TO  EL677AI.                     
00491                                                                   
091615     if eibtrnid not = trans-id
091615        evaluate true
091615           when pi-return-to-program = 'EL6315'
091615              move pi-company-cd to pi-chek-comp-cd
091615              move pi-carrier    to pi-chek-carrier
091615              move pi-grouping   to pi-chek-grouping
091615              move pi-state      to pi-chek-state
091615              move pi-account    to pi-chek-account
091615              move pi-cert-eff-dt to pi-chek-eff-dt
091615              move pi-cert-prime to pi-chek-cert-no
091615              move pi-cert-sfx   to pi-chek-suf-no
091615              move +1            to pi-chek-sequence
091615              move 'C'           to pi-check-type *> Correction
091615              move 'S'           to mainti
091615              move +1            to maintl
091615              move al-uanon      to mainta
091615              move dfhenter      to eibaid
091615              go to 5000-browse-file
091615           when pi-return-to-program = 'EL6318'
091615              move 'C'           to pi-check-type *> Correction
091615           when other
091615              move 'R'           to pi-check-type *> Refund
091615        end-evaluate
091615     end-if
00492      IF EIBTRNID NOT = TRANS-ID                                   
               move pi-processor-id    to pi-return-to
091615         move +1                 to dedcynl
               move 'N'                to dedcyno
               move al-uanon           to dedcyna
091615         move dfhpf7             to eibaid
111418         move ' '                to pi-void-reissue-pass
111418         move zeros              to pi-void-reissue-amt
091615         perform 6800-browse-previous-chek-recs *> totals up prev paid
                                       thru 6800-exit
091615         perform 5600-get-erpndb thru 5600-exit *> calc refund on pend
                                                      *> pending record
00493          IF PI-TO-EL677-FROM-EL1273                               
00494              MOVE 'S'            TO MAINTI                        
00495              MOVE 1              TO MAINTL                        
00496              MOVE AL-UABON       TO MAINTA                        
00497              MOVE DFHENTER       TO EIBAID                        
00498              GO TO 5000-BROWSE-FILE.                              
00499                                                                   
00500      IF EIBTRNID NOT = TRANS-ID                                   
00501         IF RETURNED-FROM = XCTL-1273 OR XCTL-114                 
00502            PERFORM 7200-RECOVER-TEMP-STORAGE THRU 7200-EXIT     
00503            PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
00504            IF PI-PROCESS-BENEFICIARY                            
00505               GO TO 6600-BUILD-BENEFICIARY-SCREEN               
00506            ELSE                                                 
00507               GO TO 6300-BUILD-CERT-SCREEN
                 end-if
00508         ELSE                                                     
091615           IF EIBTRNID NOT = EL6311-TRANS-ID and el6318-trans-id
00510               GO TO 8100-SEND-INITIAL-MAP                       
00511            ELSE                                                 
00512               PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
00513               IF PI-PROCESS-BENEFICIARY                         
00514                  GO TO 6600-BUILD-BENEFICIARY-SCREEN            
00515               ELSE                                              
00516                  GO TO 6300-BUILD-CERT-SCREEN
                    end-if
                 end-if
              end-if
            end-if
00517                                                                   
00518      MOVE PI-COMPANY-CD          TO  CHEK-COMPANY-CD              
00519                                      PI-CHEK-COMP-CD.             
00520                                                                   
00521      EXEC CICS HANDLE CONDITION                                   
00522          PGMIDERR  (9600-PGMID-ERROR)                             
00523          ERROR     (9990-ABEND)                                   
00524      END-EXEC.                                                    
00525                                                                   
00526      IF EIBAID = DFHCLEAR                                         
00527          GO TO 9400-CLEAR.                                        
00528                                                                   
00529      IF PI-PROCESSOR-ID = 'LGXX'                                  
00530          GO TO 0200-RECEIVE.                                      
00531                                                                   
00532      EXEC CICS READQ TS                                           
00533          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       
00534          INTO   (SECURITY-CONTROL)                                
00535          LENGTH (SC-COMM-LENGTH)                                  
00536          ITEM   (SC-ITEM)                                         
00537      END-EXEC.                                                    
00538                                                                   
00539      MOVE SC-CREDIT-DISPLAY (17)  TO PI-DISPLAY-CAP.              
00540      MOVE SC-CREDIT-UPDATE  (17)  TO PI-MODIFY-CAP.               
00541                                                                   
00542      IF NOT DISPLAY-CAP                                           
00543          MOVE 'READ'          TO SM-READ                          
00544          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           
00545          MOVE ER-0070         TO  EMI-ERROR                       
00546          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00547          GO TO 8100-SEND-INITIAL-MAP.                             
00548                                                                   
00551  0200-RECEIVE.                                                    
00552      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00553          MOVE ER-1159            TO  EMI-ERROR                    
00554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00555          MOVE -1                 TO  PFENTERL                     
00556          GO TO 8200-SEND-DATAONLY.                                
00557                                                                   
00558      EXEC CICS HANDLE CONDITION                                   
00559          MAPFAIL (8100-SEND-INITIAL-MAP)                          
00560      END-EXEC.                                                    
00561                                                                   
00562      EXEC CICS RECEIVE                                            
00563          MAP      (EL677A)                                        
00564          MAPSET   (MAPSET-NAME)                                   
00565          INTO     (EL677AI)                                       
00566      END-EXEC.                                                    
00567                                                                   
00568      IF PFENTERL GREATER ZERO                                     
00569          IF EIBAID NOT = DFHENTER                                 
00570              MOVE ER-0004        TO  EMI-ERROR                    
00571              MOVE AL-UNBOF       TO  PFENTERA                     
00572              MOVE -1             TO  PFENTERL                     
00573              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00574              GO TO 8200-SEND-DATAONLY                             
00575          ELSE                                                     
00576              IF (PFENTERI NUMERIC) AND                            
00577                 (PFENTERI GREATER 0 AND LESS 25)                  
00578                  MOVE PF-VALUES (PFENTERI)   TO  EIBAID           
00579              ELSE                                                 
00580                  MOVE ER-0029    TO  EMI-ERROR                    
00581                  MOVE AL-UNBOF   TO  PFENTERA                     
00582                  MOVE -1         TO  PFENTERL                     
00583                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00584                  GO TO 8200-SEND-DATAONLY.                        

00587  0300-CHECK-PFKEYS.                                               
00588      IF EIBAID = DFHPF23                                          
00589          GO TO 8810-PF23.                                         
00590                                                                   
00591      IF EIBAID = DFHPF24                                          
00592          GO TO 9200-RETURN-MAIN-MENU.                             
00593                                                                   
00594      IF EIBAID = DFHPF12                                          
00595          GO TO 9500-PF12.                                         
00596                                                                   
00597      IF EIBAID = DFHPF1 OR DFHPF2                                 
00598          GO TO 5000-BROWSE-FILE.                                  
00599                                                                   
           if PI-TO-EL677-FROM-EL1273
              and (eibaid not = dfhenter)
111418        and (pi-void-reissue-pass not = '1' and '2')
              go to 0320-input-error
           end-if

00600      IF EIBAID = DFHPF3                                           
00601          PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT          
00602          MOVE 'PF3'              TO  PI-PFKEY                     
00603          PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT          
00604          IF PI-RETURN-TO-PROGRAM IS EQUAL TO 'EL1273  '           
00605              GO TO 9400-CLEAR                                     
00606          ELSE                                                     
00607              MOVE XCTL-1273      TO  PGM-NAME                     
00608              GO TO 9300-XCTL.                                     
00609                                                                   
00610      IF EIBAID = DFHPF4                                           
00611          IF NOT FORCE-CAP                                         
00612              MOVE ER-0433        TO EMI-ERROR                     
00613              MOVE -1             TO PFENTERL                      
00614              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00615              GO TO 8200-SEND-DATAONLY.                            
00616                                                                   
00617      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2 OR DFHPF4 OR        
00618                              DFHPF5 OR DFHPF6 or dfhpf7
00619          GO TO 1000-EDIT-DATA.                                    
00620                                                                   
00621 *    IF EIBAID = DFHPF7                                           
00622 *        PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT          
00623 *        PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT          
00624 *        MOVE XCTL-114           TO  PGM-NAME                     
00625 *        GO TO 9300-XCTL.                                         
00626                                                                   
00627  0320-INPUT-ERROR.                                                
00628      MOVE ER-0029                TO  EMI-ERROR.                   
00629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00630      MOVE -1                     TO  PFENTERL.                    
00631      GO TO 8200-SEND-DATAONLY.                                    
00632      EJECT                                                        
00633                                                                   
00634  1000-EDIT-DATA.                                                  

111219     IF MAINTI = 'S' OR 'C' OR 'A' OR 'V' OR 'D' or 'R' or 'X'
00636          MOVE AL-UANON           TO MAINTA                        
111418         IF MAINTI = 'C' OR 'V' OR 'R'
111418             IF PI-PREV-MAINT = 'S'  OR  'C' or 'R'                      
00639                  NEXT SENTENCE                                    
00640              ELSE                                                 
00641                  MOVE ER-2056    TO  EMI-ERROR                    
00642                  MOVE -1         TO  MAINTL                       
00643                  MOVE AL-UABON   TO  MAINTA                       
00644                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00645                  GO TO 1100-EDIT-COMPLETE                         
00646          ELSE                                                     
00647              NEXT SENTENCE                                        
00648      ELSE                                                         
00649          IF EIBAID = DFHPF5 OR DFHPF6 or dfhpf7
00650             NEXT SENTENCE                                         
00651          ELSE                                                     
00652             MOVE ER-0023            TO  EMI-ERROR                 
00653             MOVE -1                 TO  MAINTL                    
00654             MOVE AL-UABON           TO  MAINTA                    
00655             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
00656                                                                   
00657      IF MODIFY-CAP                                                
00658          NEXT SENTENCE                                            
00659        ELSE                                                       
00660          IF MAINTI NOT = 'S'                                      
00661             MOVE 'UPDATE'       TO SM-READ                        
00662             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        
00663             MOVE ER-0070        TO EMI-ERROR                      
00664             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
00665             GO TO 8100-SEND-INITIAL-MAP.                          
00666                                                                   
           if (pi-return-to-program = 'EL1273')
111219        and (MAINTI not = 'S' and 'V' and 'R' and 'D' and 'X')
              move 'S'                 to mainti
              move +1                  to maintl
              move al-uanon            to mainta
           end-if

00667      IF CARRIERI NOT = LOW-VALUES                                 
00668          MOVE AL-UANON           TO  CARRIERA                     
00669          PERFORM 1200-VERIFY-CARRIER-ID THRU 1200-EXIT            
00670          MOVE CARRIERI           TO  PI-CHEK-CARRIER              
00671                                      PI-CARRIER                   
00672      ELSE                                                         
00673          MOVE -1                 TO  CARRIERL                     
00674          MOVE AL-UABON           TO  CARRIERA                     
00675          MOVE ER-0194            TO  EMI-ERROR                    
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00677                                                                   
00678      IF GROUPI NOT = LOW-VALUES                                   
00679          MOVE AL-UANON           TO  GROUPA                       
00680          MOVE GROUPI             TO  PI-CHEK-GROUPING             
00681                                      PI-GROUPING                  
00682      ELSE                                                         
00683          MOVE -1                 TO  GROUPL                       
00684          MOVE AL-UABON           TO  GROUPA                       
00685          MOVE ER-0195            TO  EMI-ERROR                    
00686          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00687                                                                   
00688      IF STATEI NOT = LOW-VALUES                                   
00689          MOVE AL-UANON           TO  STATEA                       
00690          PERFORM 1300-VERIFY-STATE-ID THRU 1390-EXIT              
00691          MOVE STATEI             TO  PI-CHEK-STATE                
00692                                      PI-STATE                     
00693      ELSE                                                         
00694          MOVE -1                 TO  STATEL                       
00695          MOVE AL-UABON           TO  STATEA                       
00696          MOVE ER-0196            TO  EMI-ERROR                    
00697          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00698                                                                   
00699      IF EFFDTL GREATER ZERO                                       
CIDMOD         MOVE EFFDTI             TO  WS-DT-DEEDIT-FIELD
CIDMOD         PERFORM 8700-DEEDIT      
CIDMOD         IF WS-DEEDIT-FIELD-DATE-OUT IS NUMERIC                         
CIDMOD             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  EFFDTO                   
CIDMOD             INSPECT EFFDTI CONVERTING SPACES TO '/'
CIDMOD             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  DC-GREG-DATE-1-MDY       
00707              MOVE '4'                TO  DC-OPTION-CODE           
00708              PERFORM 8500-DATE-CONVERT                            
00709              IF DC-ERROR-CODE NOT = SPACES                        
00710                  MOVE ER-0215        TO  EMI-ERROR                
00711                  PERFORM 9900-ERROR-FORMAT                        
00712                  MOVE -1             TO  EFFDTL                   
00713                  MOVE AL-UABON       TO  EFFDTA                   
00714                ELSE                                               
00715                  MOVE AL-UANON       TO  EFFDTA                   
00716                  MOVE DC-BIN-DATE-1  TO  PI-CHEK-EFF-DT           
00717                                          PI-CERT-EFF-DT           
00718            ELSE     
00719              MOVE ER-0215        TO  EMI-ERROR                    
00720              PERFORM 9900-ERROR-FORMAT                            
00721              MOVE -1             TO  EFFDTL                       
00722              MOVE AL-UABON       TO  EFFDTA                       
00723        ELSE                                                       
00724          MOVE ER-0216            TO  EMI-ERROR                    
00725          PERFORM 9900-ERROR-FORMAT                                
00726          MOVE -1                 TO  EFFDTL                       
00727          MOVE AL-UABOF           TO  EFFDTA.                      
00728                                                                   
00729      IF ACCTI NOT = LOW-VALUES                                    
00730          MOVE AL-UANON           TO  ACCTA                        
00731          PERFORM 1400-VERIFY-ACCOUNT THRU 1490-EXIT               
00732          MOVE ACCTI              TO  PI-CHEK-ACCOUNT              
00733                                      PI-ACCOUNT                   
00734      ELSE                                                         
00735          MOVE -1                 TO  ACCTL                        
00736          MOVE AL-UABON           TO  ACCTA                        
00737          MOVE ER-0197            TO  EMI-ERROR                    
00738          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00739                                                                   
00740      IF CERTNOI NOT = LOW-VALUES                                  
00741          MOVE AL-UANON           TO  CERTNOA                      
00742          MOVE CERTNOI            TO  PI-CHEK-CERT-NO              
00743                                      PI-CERT-PRIME                
00744      ELSE                                                         
00745          MOVE -1                 TO  CERTNOL                      
00746          MOVE AL-UABON           TO  CERTNOA                      
00747          MOVE ER-0203            TO  EMI-ERROR                    
00748          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00749                                                                   
00750      IF SFXI  = LOW-VALUES                                        
00751          MOVE SPACE              TO  SFXI.                        
00752                                                                   
00753      MOVE AL-UANON               TO  SFXA.                        
00754                                                                   
00755      MOVE SFXI                   TO  PI-CHEK-SUF-NO               
00756                                      PI-CERT-SFX.                 
00757      IF SEQI = LOW-VALUES                                         
00758          MOVE +1                 TO  SEQI.                        
00759                                                                   
00760      MOVE AL-UNNON               TO  SEQA.                        
00761      MOVE SEQI                   TO  PI-CHEK-SEQUENCE.            
00762                                                                   
00763      IF PAYTO1L > 0                                              
00764          MOVE PAYTO1I            TO  PI-PAYTO1                    
00765      ELSE                                                         
00766          MOVE SPACES             TO  PI-PAYTO1.                   
00767                                                                   
00768      IF AMOUNTL > 0                                               
00769          EXEC CICS BIF DEEDIT                                     
00770              FIELD   (AMOUNTI)                                    
00771              LENGTH  (11)                                         
00772          END-EXEC                                                 
00773          MOVE AMOUNTI            TO  PI-AMOUNT                    
00774      ELSE                                                         
00775          MOVE ZEROS              TO  PI-AMOUNT.                   
00776                                                                   
00777      IF TYPEL > 0                                                 
00778          MOVE TYPEI              TO  PI-TYPE                      
00779      ELSE                                                         
00780          MOVE SPACE              TO  PI-TYPE.                     
00781                                                                   
00782 *    IF REFL > 0                                                  
00783 *        MOVE REFI               TO  PI-REFERENCE                 
00784 *    ELSE                                                         
00785 *        MOVE SPACES             TO  PI-REFERENCE.                

           if rettol not = zeros
              move rettoi              to pi-return-to
00753         MOVE AL-UANON            TO  RETTOA
           end-if

           if dedcynl not = zeros
              if dedcyni = 'Y' OR 'N'
                 move al-uanon         to dedcyna
                 continue
              else
                 move er-3451          to emi-error
                 move -1               to dedcynl
                 move al-uabon         to dedcyna
                 perform 9900-error-format
                                       thru 9900-exit
              end-if
           end-if
00786                                                                   
111418     if mainti = 'V' or 'R'
              if ((vreasonl > zeros)
                 and (vreasoni (1:2) not = spaces))
111418             or (pi-void-reissue-pass = '1' or '2')
                 continue
              else
                 move er-3454          to emi-error
                 move -1               to vreasonl
                 move al-uabon         to vreasona
                 perform 9900-error-format
                                       thru 9900-exit
              end-if
           end-if

091615     if (mainti = 'A')
091615        and (pi-prev-paid > zeros)
091615        and (pi-check-type = 'C')
091615        move er-3459             to emi-error
091615        move -1                  to maintl
091615        move al-uabon            to mainta
091615        perform 9900-error-format
091615                                 thru 9900-exit
091615     end-if

           if mainti = 'A'
              continue
           else
00815         IF MAINTI = 'S'
                 or pi-prev-maint = 'S'
00816            GO TO 1100-EDIT-COMPLETE
              end-if
           end-if

111418     if (pi-void-reissue-pass = '1')
111418        and (mainti = 'R')
111418        if emi-error = zeros
111418           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
111418              go to 7500-reissue-pass-1
111418           else
111418              if eibaid = dfhenter
111418                 move er-3460    to emi-error
111418                 move -1         to maintl
111418                 move al-uabon   to mainta
111418                 perform 9900-error-format
111418                                 thru 9900-exit
111418              end-if
111418           end-if
111418        end-if
111418     end-if
111418
111418     if (pi-void-reissue-pass = '2')
111418        and (mainti = 'R')
111418        if emi-error = zeros
111418           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
111418              go to 7500-reissue-pass-1
111418           else
111418              if eibaid = dfhenter
111418                 go to 7510-reissue-pass-2
111418              end-if
111418           end-if
111418        end-if
111418     end-if

      *    if (pi-void-reissue-pass = '1')
      *       and (mainti = 'R')
      *       if emi-error = zeros
      *          if eibaid = dfhpf5 or dfhpf6 or dfhpf7
      *             go to 7500-reissue-pass-1
      *          else
      *             if eibaid = dfhenter
      *                go to 7510-reissue-pass-2
      *                move '2'        to pi-void-reissue-pass
      *             end-if
      *          end-if
      *       end-if
      *    end-if



00787      IF EIBAID = DFHPF5                                           
00788         IF EMI-ERROR = ZEROS
00789           GO TO 6500-BUILD-ACCOUNT-SCREEN                         
00790         ELSE                                                      
00791           GO TO 1100-EDIT-COMPLETE.                               
00792                                                                   
00793      IF EIBAID = DFHPF6                                           
00794         IF EMI-ERROR = ZEROS                                      
00795            GO TO 6300-BUILD-CERT-SCREEN                           
00796          ELSE                                                     
00797            GO TO 1100-EDIT-COMPLETE.                              
00798                                                                   
00793      IF EIBAID = DFHPF7
00794         IF EMI-ERROR = ZEROS                                      
00795            GO TO 6600-build-beneficiary-screen
00796          ELSE                                                     
00797            GO TO 1100-EDIT-COMPLETE.                              

020317     if mainti = 'A'
020317        if dedcynl not = zeros
020317           if dedcyni <> pi-previous-deduct-comm
020317              perform 6700-build-common
020317                                 thru 6700-exit
020317              MOVE WS-REFUND-AMOUNT
020317                                 TO AMOUNTO
020317              MOVE AL-UANON      TO PAYTO1A                       
020317                                    PAYTO2A                       
020317                                    PAYAD1A
020317                                    PAYCTYA
020317                                    paysta
020317                                    PTOZIPA                       
020317              GO TO 8100-send-initial-map
020317           end-if
020317        end-if
020317     end-if

      *    if mainti = 'V'
      *       if (vreasonl > zeros)
      *          and (vreasoni (1:2) not = spaces)
      *          continue
      *       else
      *          move er-3454          to emi-error
      *          move -1               to vreasonl
      *          move al-uabon         to vreasona
      *          perform 9900-error-format
      *                                thru 9900-exit
      *       end-if
      *    end-if

00799      IF MAINTI = 'A'
              if (payto1i not = spaces and low-values)
                 and (payad1i not = spaces and low-values)
                 and (payctyi not = spaces and low-values)
                 and (paysti not = spaces and low-values)
                 and (ptozipi not = spaces and low-values)
                 continue
              else
00806            MOVE -1               TO PAYTO1L                 
00807            MOVE ER-2907          TO EMI-ERROR               
00808            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              end-if
           end-if
00809                                                                   
00810      IF MAINTI = 'A'                                              
00811          IF EIBAID NOT = DFHPF4                                   
00812              MOVE DFHENTER       TO  EIBAID                       
00813              PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.      
00814                                                                   
111219     IF MAINTI = 'S' OR 'V' or 'R' or 'D' or 'X'
00816          GO TO 1100-EDIT-COMPLETE.                                
00817                                                                   
00831      IF PRINTEDL GREATER THAN +0                                  
CIDMOD         MOVE PRINTEDI           TO  WS-DEEDIT-FIELD-A            
CIDMOD         PERFORM 8700-DEEDIT                                      
CIDMOD         IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         
CIDMOD             MOVE WS-DEEDIT-FIELD-V0 TO  PRINTEDO                 
CIDMOD             INSPECT PRINTEDI CONVERTING SPACES TO '/'
CIDMOD             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY       
00839              MOVE '4'                TO  DC-OPTION-CODE           
00840              PERFORM 8500-DATE-CONVERT                            
00841              IF DC-ERROR-CODE NOT = SPACES                        
00842                  MOVE ER-3046        TO  EMI-ERROR                
00843                  PERFORM 9900-ERROR-FORMAT                        
00844                  MOVE -1             TO  PRINTEDL                 
00845                  MOVE AL-UABON       TO  PRINTEDA                 
00846                  MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT          
00847              ELSE                                                 
00848                  MOVE AL-UANON       TO  PRINTEDA                 
00849                  MOVE DC-BIN-DATE-1  TO  WS-CHK-PRINT-DT          
00850          ELSE                                                     
00851              MOVE ER-3046        TO  EMI-ERROR                    
00852              PERFORM 9900-ERROR-FORMAT                            
00853              MOVE -1             TO  PRINTEDL                     
00854              MOVE AL-UABON       TO  PRINTEDA                     
00855              MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT.             
00856                                                                   
00864      IF AMOUNTL = ZEROS
00865          GO TO 1050-CK-AMT.                                       
00866                                                                   
00867 *    EXEC CICS BIF DEEDIT                                         
00868 *        FIELD   (AMOUNTI)                                        
00869 *        LENGTH  (11)                                             
00870 *    END-EXEC.                                                    
00871                                                                   
00872      IF AMOUNTI NOT NUMERIC     OR                                
00873         AMOUNTI LESS THAN ZERO  OR                                
00874        (AMOUNTI = ZEROS  AND                                      
00875                (PI-COMPANY-ID NOT = 'LAP' AND 'RMC'))             
00876           MOVE ER-1159            TO  EMI-ERROR                   
00877           MOVE -1                 TO  AMOUNTL                     
00878           MOVE AL-UNBON           TO  AMOUNTA                     
00879           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
00880           GO TO 1100-EDIT-COMPLETE.                               
00881                                                                   
00882      MOVE AMOUNTI                TO  WS-AM-WORK.                  
00883                                                                   
00884      IF WS-AM-SIGN = SPACES                                       
00885          MOVE WS-AM-NUM          TO  WS-AMT                       
00886          ADD 0                   TO  WS-AMT                       
00887        ELSE                                                       
00888          MOVE AMOUNTI            TO  WS-AMT                       
00889          ADD 0                   TO  WS-AMT.                      
00890                                                                   
020317     IF MAINTI = 'A'
020317        IF EIBAID = DFHENTER
020317           if (ws-amt + pi-prev-paid) >
020317              (cm-lf-premium-amt + cm-lf-alt-premium-amt +
020317                 cm-ah-premium-amt)
020317              move zeros to ws-amt
020317           end-if
020317        end-if
020317     end-if

00891       MOVE WS-AMT                TO  AMOUNTO.                     
00892       MOVE AL-UNNON              TO  AMOUNTA.                     
00893                                                                   
00894  1050-CK-AMT.                                                     
00895      IF AMOUNTL = ZEROS AND                                       
00896         MAINTI  = 'A'                                             
00897          MOVE ER-1159            TO  EMI-ERROR                    
00898          MOVE -1                 TO  AMOUNTL                      
00899          MOVE AL-UNBON           TO  AMOUNTA                      
00900          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
00901                                                                   
091615*    MOVE AL-UANON              TO  TYPEA.                        

020317     if (amountl <> zeros)
020317        if (ws-amt <= zeros)
020317           and (pi-check-type <> 'C')
020317           MOVE ER-9999          TO EMI-ERROR                    
020317           MOVE -1               TO AMOUNTL                      
020317           MOVE AL-UNBON         TO AMOUNTA                      
020317           PERFORM 9900-ERROR-FORMAT
020317                                 THRU 9900-EXIT
020317        end-if
020317     end-if

00904      IF TYPEL GREATER THAN ZEROS                                  
091615         IF TYPEI = 'R' OR 'M' OR 'E' or 'C'
00906              NEXT SENTENCE                                        
00907          ELSE                                                     
00908              MOVE ER-3044    TO  EMI-ERROR                        
00909              MOVE -1         TO  TYPEL                            
091615             MOVE AL-PABON   TO  TYPEA
00911              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00912      ELSE                                                         
00913          IF MAINTI  = 'A'                                         
00914              MOVE ER-3044    TO  EMI-ERROR                        
00915              MOVE -1         TO  TYPEL                            
091615             MOVE AL-PABON   TO  TYPEA                            
00917              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
00918                                                                   
01000                                                                   
01001  1100-EDIT-COMPLETE.                                              
01002                                                                   
01003      IF EMI-FATAL-CTR GREATER THAN +0  OR                         
01004        (EMI-FORCABLE-CTR GREATER THAN +0  AND                     
01005         EIBAID NOT = DFHPF4)                                      
01006          GO TO 8200-SEND-DATAONLY.                                
01007                                                                   
01008      MOVE MAINTI                 TO  PI-PREV-MAINT.               
01009                                                                   
01010      IF MAINTI = 'A'                                              
01011          GO TO 2000-ADD-RECORD.                                   
01012                                                                   
01013 *    IF MAINTI = 'C' OR 'V'                                       
111219     IF MAINTI = 'V' or 'R' or 'X'
01014          GO TO 3000-CHANGE-RECORD.                                
01015                                                                   
01016      IF MAINTI = 'S' or 'C'
01017          GO TO 5000-BROWSE-FILE.                                  
01018                                                                   
           if mainti = 'D'
              go to 3400-delete-record
           end-if

           .
01021  1200-VERIFY-CARRIER-ID.                                          

01022      MOVE SPACES                 TO ELCNTL-KEY.                   
01023      MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.               
01024      MOVE '6'                    TO ELCNTL-REC-TYPE.              
01025      MOVE CARRIERI               TO ELCNTL-CARRIER.               
01026      MOVE +0                     TO ELCNTL-SEQ.                   
01027                                                                   
01028      EXEC CICS HANDLE CONDITION                                   
01029          NOTFND   (1290-NO-CARRIER)                               
01030      END-EXEC.                                                    
01031                                                                   
01032      EXEC CICS READ                                               
01033          DATASET   (ELCNTL-FILE-ID)                               
01034          SET       (ADDRESS OF CONTROL-FILE)                      
01035          RIDFLD    (ELCNTL-KEY)                                   
01036      END-EXEC.                                                    
01037                                                                   
01038      GO TO 1200-EXIT.                                             
01039                                                                   
01040  1290-NO-CARRIER.                                                 
01041      MOVE ER-2208                TO EMI-ERROR.                    
01042      MOVE -1                     TO CARRIERL.                     
01043      MOVE AL-UABON               TO CARRIERA.                     
01044      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01045                                                                   
01046  1200-EXIT.                                                       
01047       EXIT.                                                       
01048      EJECT                                                        

01078  1300-VERIFY-STATE-ID.                                            
01079      MOVE SPACES                 TO ELCNTL-KEY.                   
01080      MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.               
01081      MOVE '3'                    TO ELCNTL-REC-TYPE.              
01082      MOVE STATEI                 TO ELCNTL-STATE.                 
01083      MOVE +0                     TO ELCNTL-SEQ.                   
01084                                                                   
01085      EXEC CICS HANDLE CONDITION                                   
01086          NOTFND   (1380-NO-STATE)                                 
01087      END-EXEC.                                                    
01088                                                                   
01089      EXEC CICS READ                                               
01090          DATASET   (ELCNTL-FILE-ID)                               
01091          SET       (ADDRESS OF CONTROL-FILE)                      
01092          RIDFLD    (ELCNTL-KEY)                                   
01093      END-EXEC.                                                    
01094                                                                   
01095      GO TO 1390-EXIT.                                             
01096                                                                   
01097  1380-NO-STATE.                                                   
01098      MOVE ER-2209                TO EMI-ERROR.                    
01099      MOVE -1                     TO STATEL.                       
01100      MOVE AL-UABON               TO STATEA.                       
01101      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01102                                                                   
01103  1390-EXIT.                                                       
01104       EXIT.                                                       
01105      EJECT                                                        
01106                                                                   
01107  1400-VERIFY-ACCOUNT.                                             
01108      IF EIBAID = DFHPF5                                           
01109          GO TO 1410-BUILD-ACCOUNT-KEY.                            
01110                                                                   
01111      IF MAINTL GREATER +0                                         
01112          NEXT SENTENCE                                            
01113        ELSE                                                       
01114          GO TO 1490-EXIT.                                         
01115                                                                   
01116 *    IF MAINTI = 'A' OR 'C' OR 'V'                                
111418     IF MAINTI = 'A' OR 'V' or 'R'                         
01117          NEXT SENTENCE                                            
01118        ELSE                                                       
01119          GO TO 1490-EXIT.                                         
01120                                                                   
01121  1410-BUILD-ACCOUNT-KEY.                                          
01122      MOVE CARRIERI               TO ERACCT-CARRIER.               
01123      MOVE GROUPI                 TO ERACCT-GROUPING               
01124      MOVE STATEI                 TO ERACCT-STATE.                 
01125      MOVE ACCTI                  TO ERACCT-ACCOUNT.               
01126      MOVE PI-COMPANY-CD          TO ERACCT-CO.                    
01127                                                                   
01128      MOVE '2'                    TO DC-OPTION-CODE.               
01129      MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.          
01130                                                                   
01131      PERFORM 8500-DATE-CONVERT.                                   
01132      IF DATE-CONVERSION-ERROR                                     
01133         GO TO 1480-ACCOUNT-INVALID.                               
01134                                                                   
01135      MOVE DC-BIN-DATE-1          TO ERACCT-EXP-DATE.              

          .
       1420-get-eracct.

01137      MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.              
01138                                                                   
01139      EXEC CICS HANDLE CONDITION                                   
01140          NOTFND   (1480-ACCOUNT-INVALID)                          
01141          ENDFILE  (1480-ACCOUNT-INVALID)                          
01142      END-EXEC.                                                    
01143                                                                   
01144      EXEC CICS STARTBR                                            
01145          DATASET   (ERACCT-FILE-ID)                               
01146          RIDFLD    (ERACCT-KEY)                                   
01147      END-EXEC.                                                    
01148                                                                   
           set eracct-start to true
           .
01149  1420-READNEXT-ACCOUNT-MASTER.                                    
01150      EXEC CICS READNEXT                                           
01151          DATASET   (ERACCT-FILE-ID)                               
01152          SET       (ADDRESS OF ACCOUNT-MASTER)                    
01153          RIDFLD    (ERACCT-KEY)                                   
01154      END-EXEC.                                                    
01155                                                                   
01156 *    IF PI-COMPANY-CD    NOT = AM-COMPANY-CD   OR                 
01157 *       SV-ACCT-GROUPING NOT = AM-GROUPING     OR                 
01158 *       SV-ACCT-CARRIER  NOT = AM-CARRIER      OR                 
01159 *       SV-ACCT-STATE    NOT = AM-STATE        OR                 
01160 *       SV-ACCT-ACCOUNT  NOT = AM-ACCOUNT                         
01161 *         GO TO 1480-ACCOUNT-INVALID.                             
01162 *                                                                 
01163 *    IF SV-ACCT-EXP-DATE LESS AM-EFFECTIVE-DT                     
01164 *        GO TO 1480-ACCOUNT-INVALID.                              
01165 *                                                                 
01166 *    IF SV-ACCT-EXP-DATE NOT LESS AM-EXPIRATION-DT                
01167 *        GO TO 1420-READNEXT-ACCOUNT-MASTER.                      
01168                                                                   
           evaluate true
              when (save-eracct-key (1:20) not =
                 am-control-primary (1:20))
                 and (not eracct-found)
                 go to 1480-account-invalid
              when (save-eracct-key (1:20) not =
                 am-control-primary (1:20))
                 and (eracct-found)
                 move ws-hold-eracct-record
                                       to account-master
              when (sv-acct-exp-date < am-effective-dt)
                 and (not eracct-found)
                 go to 1480-account-invalid
              when sv-acct-exp-date >= am-expiration-dt
                 go to 1420-readnext-account-master
              when am-expiration-dt = high-values
                 continue
              when other
                 set eracct-found to true
                 move account-master   to ws-hold-eracct-record
                 go to 1420-readnext-account-master
           end-evaluate

           if eracct-start
              exec cics endbr
                 dataset  (eracct-file-id)
              end-exec
           end-if

      *    if am-expiration-dt = high-values
      *       continue
      *    else
      *       move account-master      to ws-hold-eracct-record
      *       go to 1420-readnext-account-master
      *    end-if

01169      MOVE AM-CARRIER             TO PI-CR-CARRIER.                
01170      MOVE AM-GROUPING            TO PI-CR-GROUPING.               
01171      MOVE AM-STATE               TO PI-CR-STATE.                  
01172      MOVE AM-AGT (AM-REMIT-TO)   TO PI-CR-FIN-RESP.               
01173                                                                   
01174      MOVE AM-NAME                TO PI-AM-NAME.                   
01175      MOVE AM-ADDRS               TO PI-AM-ADDRS.                  
01176      MOVE AM-CITY                TO PI-AM-CITY-st
01177      MOVE AM-ZIP                 TO PI-AM-ZIP-CODE.
           move am-csr-code            to pi-am-csr               
01178                                                                   
01179      MOVE +0                     TO WS-SUB.                       
01180                                                                   
01181  1430-FIND-ACC-AGT.                                               
01182      ADD +1                      TO WS-SUB.                       
01183                                                                   
01184      IF  WS-SUB GREATER +10                                       
01185          GO TO 1480-ACCOUNT-INVALID.                              
01186                                                                   
052814     IF (AM-COM-TYP (WS-SUB) = 'C' OR 'D' OR 'F')
01188          MOVE AM-AGT (WS-SUB)    TO  PI-CR-ACCOUNT                
01189        ELSE                                                       
01190          GO TO 1430-FIND-ACC-AGT.                                 
01191                                                                   
01192      MOVE AL-UANON               TO CARRIERA                      
01193                                     GROUPA                        
01194                                     STATEA.                       
01195                                                                   
01196      PERFORM 6000-VERIFY-COMP-MASTER THRU 6090-EXIT.              
01197                                                                   
01198      GO TO 1490-EXIT.                                             
01199                                                                   
01200  1480-ACCOUNT-INVALID.                                            
01201      MOVE -1                     TO CARRIERL                      
01202      MOVE AL-UANON               TO CARRIERA                      
01203                                     GROUPA                        
01204                                     STATEA                        
01205                                     ACCTA.                        
01206      MOVE ER-2210                TO EMI-ERROR.                    
01207      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01208      GO TO 8200-SEND-DATAONLY.                                    
01209                                                                   
01210  1490-EXIT.                                                       
01211       EXIT.                                                       
01212                                                                   
01213      EJECT                                                        
01214  1500-VERIFY-PENDING-BUS-REC.                                     

01219      MOVE 'N'                    TO WS-PNDB-FOUND-SW.             

091615     if pi-corr-check
091615        MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW
091615        go to 1590-exit
091615     end-if

01215      EXEC CICS HANDLE CONDITION                                   
01216          NOTFND   (1590-EXIT)                                     
01217      END-EXEC.                                                    
01218                                                                   
01220                                                                   
01221      MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.        
01222      MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.           
01223      MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.          
01224      MOVE PI-STATE               TO ERPNDB-ALT-STATE.             
01225      MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.           
01226      MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.        
01227      MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.          
01228      MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.       
01229      MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.         
01230      MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.       
01231                                                                   
01232      IF ST-ACCNT-CNTL  OR                                         
01233         ACCNT-CNTL                                                
01234           MOVE SPACES             TO  ERPNDB-ALT-CARRIER.         
01235                                                                   
01236      IF ST-ACCNT-CNTL      OR                                     
01237         CARR-ST-ACCNT-CNTL OR                                     
01238         ACCNT-CNTL         OR                                     
01239         CARR-ACCNT-CNTL                                           
01240           MOVE SPACES             TO  ERPNDB-ALT-GROUPING.        
01241                                                                   
01242      IF ACCNT-CNTL OR                                             
01243         CARR-ACCNT-CNTL                                           
01244           MOVE SPACES             TO  ERPNDB-ALT-STATE.           
01245                                                                   
01246      EXEC CICS READ                                               
01247           DATASET   (ERPNDB-ALT-FILE-ID)                          
01248           SET       (ADDRESS OF PENDING-BUSINESS)                 
01249           RIDFLD    (ERPNDB-ALT-KEY)                              
01250       END-EXEC.                                                   
01251                                                                   
01252      MOVE 'Y'                    TO WS-PNDB-FOUND-SW.             
01253                                                                   
01254      IF PB-C-PAYEE-CODE GREATER SPACES                            
01255         MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW.    
01256                                                                   
01257      MOVE PB-C-REFERENCE         TO PI-REFERENCE.                 
01258 *    MOVE PB-CI-INSURED-NAME     TO PI-INSURED-NAME.              
01259                                                                   
01260      MOVE PB-C-LF-CANCEL-AMT     TO  PI-LF-REFUND.                
01261      MOVE PB-C-AH-CANCEL-AMT     TO  PI-AH-REFUND.                
01262                                                                   
01263      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        
01264          MOVE PB-C-LF-CANCEL-DT  TO  PI-CANC-DT                   
01265      ELSE                                                         
01266          MOVE PB-C-AH-CANCEL-DT  TO  PI-CANC-DT.                  
01267                                                                   
01268 *    IF MAINTI = 'A' OR 'C'                                       
01269 *      IF PB-C-REFUND-CREATED                                     
01270 *          MOVE -1               TO  MAINTL                       
01271 *          MOVE ER-3445          TO  EMI-ERROR                    
01272 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
01273                                                                   
01274  1590-EXIT.                                                       
01275       EXIT.                                                       
01276                                                                   
01277      EJECT                                                        
01278  2000-ADD-RECORD.                                                 

01279      EXEC CICS GETMAIN                                            
01280          SET      (ADDRESS OF CHECK-RECORDS)                      
01281          LENGTH   (600)                                           
01282          INITIMG  (GETMAIN-SPACE)                                 
01283      END-EXEC.                                                    
01284                                                                   
01285      MOVE 'CH'                   TO CH-RECORD-ID.                 
01286      MOVE PI-COMPANY-CD          TO CH-COMPANY-CD.                
01287      MOVE CARRIERI               TO CH-CARRIER.                   
01288      MOVE GROUPI                 TO CH-GROUPING.                  
01289      MOVE STATEI                 TO CH-STATE.                     
01290      MOVE ACCTI                  TO CH-ACCOUNT.                   
01291                                                                   
01292      MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.          
01293      MOVE '2'                    TO DC-OPTION-CODE.               
01294      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01295      MOVE DC-BIN-DATE-1          TO CH-CERT-EFF-DT.               
01296                                                                   
01297      MOVE CERTNOI                TO CH-CERT-PRIME.                
01298      MOVE SFXI                   TO CH-CERT-SFX.                  
01299                                                                   
01300      MOVE SEQI                   TO CH-SEQUENCE-NO.               
01301                                                                   
01302      MOVE PI-PROCESSOR-ID        TO CH-RECORDED-BY
                                          ch-released-by
01303      MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.         
01304                                                                   
01305      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
01306      MOVE '5'                    TO DC-OPTION-CODE.               
01307      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01308      MOVE DC-BIN-DATE-1          TO CH-RECORDED-DT
                                          ch-released-dt
01309                                                                   
           move 'P'                    to ch-approval-status
01311                                                                   
01312      MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT           
01313                                     CH-VOID-DT                    
01314                                     CH-CREDIT-ACCEPT-DT           
01315                                     CH-CANC-DT
                                          ch-approval-dt
                                          ch-check-cashed-dt

01316      MOVE ZEROS                  TO CH-LF-REFUND                  
01317                                     CH-AH-REFUND                  
01318 *                                   CH-DEDUCT-WITHHELD            
01319 *                                   CH-ADDITIONAL-CHARGE.         
01320                                                                   
01321      PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.                      
01322                                                                   
           if pi-prev-paid not numeric
              move zeros               to pi-prev-paid
           end-if

103116     if pi-prev-paid-this-month not numeric
103116        move zeros               to pi-prev-paid-this-month
103116     end-if
01354      IF EMI-FATAL-CTR GREATER THAN +0  OR                         
01355        (EMI-FORCABLE-CTR GREATER THAN +0  AND                     
01356         EIBAID NOT = DFHPF4)                                      
01357          GO TO 8200-SEND-DATAONLY.                                
01358                                                                   
           add ch-amount-paid to pi-prev-paid
01359      MOVE PI-CR-MONTH-END-DT     TO CH-CREDIT-SELECT-DT.          
01360      MOVE PI-CR-CARRIER          TO CH-COMP-CARRIER.              
01361      MOVE PI-CR-GROUPING         TO CH-COMP-GROUPING.             
01362      MOVE PI-CR-FIN-RESP         TO CH-COMP-FIN-RESP.             
01363      MOVE PI-CR-ACCOUNT          TO CH-COMP-ACCOUNT.
           move pi-am-csr              to ch-csr

           .
01366  2100-WRITE-RECORD.                                               

           exec cics read
              dataset    (erchek-file-id)
              into       (ws-dummy-erchek)
              ridfld     (ch-control-primary)
091615        resp       (ws-response)
           end-exec

           if RESP-NOTFND
              continue
           else
              add +1 to ch-sequence-no
              add 1 to seqi
              go to 2100-write-record
           end-if

           PERFORM 2700-WRITE-SQL      THRU 2700-EXIT

01367      EXEC CICS HANDLE CONDITION
01368          DUPREC (2200-DUPREC)                                     
01369      END-EXEC.                                                    
01370                                                                   
01371      MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.                
01372      MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL          
01373                                     CH-CHECK-QUE-SEQUENCE
01374                                                                   
01375      EXEC CICS WRITE                                              
01376          DATASET  (ERCHEK-FILE-ID)                                
01377          FROM     (CHECK-RECORDS)                                 
01378          RIDFLD   (CH-CONTROL-PRIMARY)                            
091615         resp     (ws-response)
01379      END-EXEC.                                                    

091615     if resp-normal and pi-check-type = 'C'
091615        move 'Y'                 to pi-check-cut
091615     end-if

01381      COMPUTE WS-JOURNAL-RECORD-LENGTH =                           
01382              ERCHEK-RECORD-LENGTH + 23.                           
01383                                                                   
091615     if pi-check-type not = 'C'
              PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT
              if resp-normal
                 MOVE 'Y'              TO PB-C-REFUND-SW
                 add +1 to pi-chek-rec-cnt
                 perform 6210-rewrite-pndb
                                       thru 6210-exit
      *          PERFORM 2700-WRITE-SQL THRU 2700-EXIT
              end-if
091615     end-if

01388      IF EMI-NO-ERRORS                                             
01389          MOVE ER-0000            TO EMI-ERROR                     
01390      ELSE                                                         
01391          IF EMI-FORCABLE-CTR GREATER THAN +0  AND                 
01392             EIBAID = DFHPF4                                       
01393              MOVE ER-2600        TO EMI-ERROR.                    
01394                                                                   
01395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01396                                                                   
01397      MOVE LOW-VALUES             TO EL677AI.                      
01398                                                                   
01399      MOVE PI-CHEK-CARRIER        TO CARRIERO.                     
01400      MOVE PI-CHEK-GROUPING       TO GROUPO.                       
01401      MOVE PI-CHEK-STATE          TO STATEO.                       
01402      MOVE PI-CHEK-ACCOUNT        TO ACCTO.                        
01403                                                                   
01404      MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.                
01405      MOVE SPACE                  TO DC-OPTION-CODE.               
01406      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01407      MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.                       
01408                                                                   
01409      MOVE PI-CHEK-CERT-NO        TO CERTNOO.                      
01410      MOVE PI-CHEK-SUF-NO         TO SFXO.                         
01411      MOVE PI-CHEK-SEQUENCE       TO SEQO.                         
01412                                                                   
01413      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA      
01414                                     ACCTA     EFFDTA  CERTNOA     
01415                                     SFXA.                         
01416      MOVE AL-UNNON               TO SEQA.                         
           go to 5000-browse-file
01417      GO TO 8100-SEND-INITIAL-MAP.                                 
01418                                                                   
01419  2200-DUPREC.                                                     
01420      ADD +1 TO CH-SEQUENCE-NO.                                    
01421      MOVE CH-SEQUENCE-NO         TO SEQO                          
01422                                     PI-CHEK-SEQUENCE              
01423                                     CHEK-RECORD-SEQ.              
01424      GO TO 2100-WRITE-RECORD.                                     

       2700-WRITE-SQL.

           perform 2710-build-rec      thru 2710-exit
           perform 2720-insert-row     thru 2720-exit
           perform 4300-FINISH-UP-DB   thru 4300-exit

           .
       2700-EXIT.
           EXIT.

       2710-BUILD-REC.

           move spaces                 to daily-check-request-rec
           move pi-company-id          to db-compid
           move ch-carrier             to db-carrier
                                          db-fincar
           move ch-grouping            to db-grouping
                                          db-fingrp
           move ch-state               to db-state
           move ch-account             to db-account
           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to db-effdate
           else
              move spaces              to db-effdate
           end-if
           move ch-cert-prime          to db-certificate
           move ch-cert-sfx            to db-cert-sfx
           move ch-sequence-no         to db-seq-no
           move '1'                    to db-type
091615                                    db-check-sub-type
091615     if pi-check-type = 'C'
091615        move '2'                 to db-check-sub-type
091615     end-if
           move ch-amount-paid         to db-amount-n
           move zeros                  to db-checkstatus
           move ch-check-que-control   to db-releasebatch
           move save-bin-date          to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to db-releasedt
           else
              move spaces              to db-releasedt
           end-if
           move pi-processor-id        to db-releaseby
           move ch-payee-name-1        to db-payeename1
           move ch-payee-name-2        to db-payeename2
           move ch-payee-address-1     to db-payeeaddr1
           move ch-payee-address-2     to db-payeeaddr2
           move ch-payee-city          to db-payeecity
           move ch-payee-state         to db-payeest
           move ch-payee-zip-code      to db-payeezip
           move ch-comp-fin-resp       to db-finresp
           move ch-comp-account        to db-finacct
           move ch-recorded-by         to db-preparer
           move ch-return-to           to db-return-to
           move pi-table-name          to db-insured-name

           .
       2710-EXIT.
           EXIT.

       2720-INSERT-ROW.

           if not connected-to-db
              perform 4100-CONNECT-TO-DB
                                       thru 4100-exit
           end-if

           EXEC SQL
              INSERT into ChkApp_Check (
                 Company,
                 CertCarrier,
                 CertGroup,
                 CertState,
                 CertAccount,
                 CertEffDate,
                 CertNumber,
                 CertNumberSuf,
                 CheckSeqNbr,
                 CheckType,
                 CheckAmount,
                 ReleaseBatchNbr,
                 ReleaseDate,
                 ReleasedBy,
                 PayeeName1,
                 PayeeName2,
                 PayeeAddress1,
                 PayeeAddress2,
                 PayeeCity,
                 PayeeState,
                 PayeeZip,
                 CompCarrier,
                 CompGroup,
                 CompFinResp,
                 CompAccount,
                 Preparer,
                 ReturnTo,
091615           InsuredName,
091615           CheckSubType)
               VALUES (
                 :DB-CompId,
                 :DB-Carrier,
                 :DB-Grouping,
                 :DB-State,
                 :DB-Account,
                 :DB-EffDate,
                 :DB-Certificate,
                 :db-cert-sfx,
                 :DB-Seq-No,
                 :DB-Type,
                 :db-amount,
                 :db-releasebatch,
                 :db-releasedt,
                 :db-releaseby,
                 :db-payeename1,
                 :db-payeename2,
                 :db-payeeaddr1,
                 :db-payeeaddr2,
                 :db-payeecity,
                 :db-payeest,
                 :db-payeezip,
                 :db-fincar,
                 :db-fingrp,
                 :db-finresp,
                 :db-finacct,
                 :db-preparer,
                 :db-return-to,
091615           :db-insured-name,
091615           :db-check-sub-type)
           END-EXEC

      *    display ' about to insert '
      *    display ' db-compid     ' db-compid
      *    display ' db-carrier    ' db-carrier
      *    display ' db-group      ' db-grouping
      *    display ' db state      ' db-state
      *    display ' accounbt      ' db-account
      *    display ' effdt         ' db-effdate
      *    display ' cert          ' db-certificate
      *    display ' suffix        ' db-cert-sfx
      *    display ' seq no        ' db-seq-no
      *    display ' type          ' db-type
           if sqlcode = -2601
              add +1                   to ch-sequence-no
              move ch-sequence-no      to DB-Seq-No
              go to 2720-insert-row
           end-if

           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              string ws-dis-sql-code ' ' sqlerrmc (1:50)
                 into EMI-MESSAGE-AREA (1)
              end-string
              display ' msga1 ' emi-message-area (1)
              perform 4300-FINISH-UP-DB thru 4300-exit
              go to 8200-send-dataonly
      *       goback
           end-if

           .
       2720-EXIT.
           EXIT.

01666  3000-CHANGE-RECORD.                                              
01667      EXEC CICS HANDLE CONDITION                                   
01668          NOTFND (3900-RECORD-NOTFND)                              
01669      END-EXEC.                                                    

111219     if mainti <> 'X'
111219        go to 3000-continue
111219     end-if
111219
111219     move pi-erchek-key          to erchek-key
111219
111219     exec cics startbr
111219         dataset     ('ERCHEK')
111219         ridfld      (erchek-key)
111219         resp        (ws-response)
111219     end-exec
111219
111219     if not resp-normal
111219        display ' bad startbr ' ws-response
111219        go to 3000-continue
111219     end-if
111219
111219     .
111219 3000-readnext.
111219
111219     exec cics readnext
111219        dataset    ('ERCHEK')
111219        ridfld  (erchek-key)
111219        set     (address of check-records)
111219        resp    (ws-response)
111219     end-exec
111219
111219     if not resp-normal
111219        display ' bad readnext ' ws-response
111219        go to 3000-endbr
111219     end-if
111219
111219     if ch-company-cd = pi-company-cd and
111219        ch-carrier    = pi-chek-carrier and
111219        ch-state      = pi-chek-state and
111219        ch-account    = pi-chek-account and
111219        ch-cert-eff-dt = pi-chek-eff-dt and
111219        ch-cert-prime = pi-chek-cert-no and
111219        ch-cert-sfx   = pi-chek-suf-no
111219        continue
111219     else
111219        go to 3000-endbr
111219     end-if
111219     
111219     if ch-amount-paid = pi-void-reissue-amt and
111219        ch-void-dt = low-values and 
111219        ch-recorded-dt = save-bin-date
111219        move er-3463             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     else
111219        go to 3000-readnext
111219     end-if
111219
111219     .
111219 3000-endbr.
111219
111219     exec cics endbr
111219        dataset   ('ERCHEK')
111219     end-exec
111219
111219     .
111219 3000-continue.
01670                                                                   
01671      EXEC CICS READ                                               
01672          DATASET  (ERCHEK-FILE-ID)                                
01673          SET      (ADDRESS OF CHECK-RECORDS)                      
01674          RIDFLD   (PI-ERCHEK-KEY)                                 
01675          UPDATE                                                   
01676      END-EXEC.                                                    
01677                                                                   
01678      IF CH-LF-REFUND NOT NUMERIC                                  
01679          MOVE ZEROS              TO CH-LF-REFUND.                 
01680      IF CH-AH-REFUND NOT NUMERIC                                  
01681          MOVE ZEROS              TO CH-AH-REFUND.                 
01682 *    IF CH-DEDUCT-WITHHELD NOT NUMERIC                            
01683 *        MOVE ZEROS              TO CH-DEDUCT-WITHHELD.           
01684 *    IF CH-ADDITIONAL-CHARGE NOT NUMERIC                          
01685 *        MOVE ZEROS              TO CH-ADDITIONAL-CHARGE.         
01686                                                                   
01687      IF VREASONL NOT = ZEROS                                      
01688          MOVE VREASONI           TO  CH-VOID-REASON.              
01689                                                                   
111418     IF MAINTI = 'V' or 'R'
01691         GO TO 3050-void-processing
           end-if             

           .
111219 3010-VOID-REVERSAL.  *>  Mainti of X only
111219
111219     if ch-void-dt = low-values
111219        move er-3462             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        exec cics unlock
111219           dataset    (ERCHEK-FILE-ID)
111219        end-exec 
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     end-if
111219
111219     if ch-void-dt <> SAVE-BIN-DATE
111219        move er-3461             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        exec cics unlock
111219           dataset    (ERCHEK-FILE-ID)
111219        end-exec 
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     end-if
111219
111219     move low-values             to ch-void-dt
111219     move spaces                 to ch-void-by
111219                                    ch-void-reason
111219     
111219     go to 3100-rewrite-record

01693      PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.              
01694                                                                   
01695      PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.                      
01696                                                                   
01705      IF EMI-FATAL-CTR GREATER THAN +0  OR                         
01706        (EMI-FORCABLE-CTR GREATER THAN +0  AND                     
01707         EIBAID NOT = DFHPF4)                                      
01708          GO TO 8200-SEND-DATAONLY.                                

           go to 3100-rewrite-record

           .
       3050-void-processing.

           perform 4100-CONNECT-TO-DB  thru 4100-exit
           if sqlcode = 0
              perform 4200-get-tbl-row thru 4200-exit
              if sqlcode = 0
                 if nu-app-date = -1
                    move -1            to maintl
                    move er-3453       to emi-error
                    move al-uabon      to mainta
                    perform 9900-error-format
                                       thru 9900-exit
                    perform 4300-FINISH-UP-DB thru 4300-exit
                    go to 8200-send-dataonly
                 end-if
              end-if
              if connected-to-db
                 perform 4300-FINISH-UP-DB thru 4300-exit
              end-if
           end-if

           if (ch-check-written-dt = low-values)
              or (ch-check-no = spaces)
              move -1                  to maintl
              move er-3455             to emi-error
              move al-uabon            to mainta
              perform 9900-error-format
                                       thru 9900-exit
              perform 4300-FINISH-UP-DB thru 4300-exit
              go to 8200-send-dataonly
030414     end-if

           .
01712  3100-REWRITE-RECORD.                                             
01713      MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.         
01714                                                                   
01715      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
01716      MOVE '5'                    TO DC-OPTION-CODE.               
01717      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01718                                                                   
01719      IF MAINTI = 'C'                                              
01720        MOVE PI-PROCESSOR-ID      TO CH-RECORDED-BY
           end-if

111418     IF MAINTI = 'V' or 'R'                                       
030414        IF (CH-VOID-DT = LOW-VALUES)
030414           and (pi-check-cashed = spaces)
01726            MOVE PI-PROCESSOR-ID  TO CH-VOID-BY                    
01727            MOVE DC-BIN-DATE-1    TO CH-VOID-DT                    
01729            PERFORM 3200-PAY-ADJS-REVERSAL      THRU 3290-EXIT     
                 if pi-chek-rec-cnt = +1
01730               PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT     
                    if resp-normal
                       MOVE ' '        TO PB-C-REFUND-SW
                       perform 6210-rewrite-pndb
                                       thru 6210-exit
                    end-if
                 end-if
01731         ELSE                                                      
01732            MOVE -1               TO MAINTL                        
01733            MOVE ER-2583          TO EMI-ERROR                     
01734            MOVE AL-UABON         TO MAINTA                        
01735            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01736            GO TO 8200-SEND-DATAONLY
              end-if
           end-if
01737                                                                   
01738      MOVE CH-AMOUNT-PAID         TO WS-SV-AMOUNT-PAID.            
01739                                                                   
01740      EXEC CICS REWRITE                                            
01741          DATASET  (ERCHEK-FILE-ID)                                
01742          FROM     (CHECK-RECORDS)                                 
01743      END-EXEC.                                                    
01744                                                                   
01745      IF (PI-COMPANY-ID EQUAL 'LGX' OR 'TMS') AND                  
01746         (MAINTI EQUAL 'C')          AND                           
01747         (PI-PAYTO1 NOT EQUAL PAYTO1I)                             
01748             PERFORM 3300-UPDATE-ERPYAJ THRU 3300-EXIT.            

111219     if mainti = 'X'
111219        perform 3320-remove-rev-erpyaj-rec
111219                                 thru 3320-exit
111219     end-if

01750      IF EMI-NO-ERRORS                                             
01751          MOVE ER-0000            TO EMI-ERROR                     
01752      ELSE                                                         
01753          IF EMI-FORCABLE-CTR GREATER THAN +0  AND                 
01754             EIBAID = DFHPF4                                       
01755              MOVE ER-2600        TO EMI-ERROR.                    

111418     if mainti = 'R'
111418        if pi-void-reissue-pass = ' '
111418           move '1'              to pi-void-reissue-pass
111418        MOVE ER-3274             TO EMI-ERROR
111418        end-if
111418     end-if

01757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01758                                                                   
01759      MOVE LOW-VALUES             TO EL677AI.                      
01760                                                                   
01761      MOVE PI-CHEK-CARRIER        TO CARRIERO.                     
01762      MOVE PI-CHEK-GROUPING       TO GROUPO.                       
01763      MOVE PI-CHEK-STATE          TO STATEO.                       
01764      MOVE PI-CHEK-ACCOUNT        TO ACCTO.                        
01765                                                                   
01766      MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.                
01767      MOVE SPACE                  TO DC-OPTION-CODE.               
01768      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01769      MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.                       
01770                                                                   
01771      MOVE PI-CHEK-CERT-NO        TO CERTNOO.                      
01772      MOVE PI-CHEK-SUF-NO         TO SFXO.                         
01773      MOVE PI-CHEK-SEQUENCE       TO SEQO.                         
01774                                                                   
01775      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA      
01776                                      ACCTA   EFFDTA      
01777                                     CERTNOA   SFXA.               
01778                                                                   
01779      MOVE AL-UNNON               TO SEQA.                         
           go to 5000-browse-file
01780 *    GO TO 8100-SEND-INITIAL-MAP.                                 
01781                                                                   
           .
01783  3200-PAY-ADJS-REVERSAL.                                          
01784                                                                   
01785      IF CH-AMOUNT-PAID NOT = ZEROS                                
01786          PERFORM 3250-BUILD-ERPYAJ-REVERSAL THRU 3259-BUILD-EXIT  
01787          PERFORM 3280-WRITE-ERPYAJ-REVERSAL THRU 3289-WRITE-EXIT. 
01788                                                                   
01790      GO TO 3290-EXIT

           .
01827  3250-BUILD-ERPYAJ-REVERSAL.                                      
01828                                                                   
01829      EXEC CICS GETMAIN                                            
01830          SET     (ADDRESS OF PENDING-PAY-ADJ)                     
01831          LENGTH  (200)                                            
01832          INITIMG (GETMAIN-SPACE)                                  
01833      END-EXEC.                                                    
01834                                                                   
01835      MOVE 'PY'                   TO PY-RECORD-ID.                 
01836      MOVE PI-COMPANY-CD          TO PY-COMPANY-CD.                
01837                                                                   
01838      MOVE CH-COMP-CARRIER        TO PY-CARRIER.                   
01839      IF PI-ZERO-CARRIER  OR                                       
01840         PI-ZERO-CAR-GROUP                                         
01841          MOVE ZERO               TO PY-CARRIER.                   
01842                                                                   
01843      MOVE CH-COMP-GROUPING       TO PY-GROUPING.                  
01844      IF PI-ZERO-GROUPING  OR                                      
01845         PI-ZERO-CAR-GROUP                                         
01846          MOVE ZERO               TO PY-GROUPING.                  
01847                                                                   
01848      MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.                  
01849      MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.                   
030414     MOVE 'R'                    TO PY-RECORD-TYPE.               
01851      MOVE EIBTIME                TO PY-FILE-SEQ-NO.               

           move '1825011300'           to py-gl-account
           move 'VOID REFCK'           TO py-gl-comment
030414     move ch-amount-paid         to py-entry-amt
030414*    COMPUTE PY-ENTRY-AMT = CH-AMOUNT-PAID * -1.                  
01873                                                                   
01874      MOVE CH-CHECK-NO            TO WS-CHECK-WORK                 
01875      MOVE WS-CHECK-NO            TO PY-CHECK-NUMBER.              
01876                                                                   
01877      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             
01878      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.         
01879      MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT              
01880                                     PY-INPUT-DT.                  
01881 *    MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL          
01882 *                                   PY-CHECK-QUE-SEQUENCE.        
01883      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT           
01884                                     PY-BILLED-DATE                
01885                                     PY-REPORTED-DT                
01886                                     PY-CHECK-WRITTEN-DT           
01887                                     PY-AR-DATE                    
01888                                     PY-GL-DATE.                   
01889      MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.          
01890      MOVE CH-CHECK-ORIGIN-SW     TO PY-CHECK-ORIGIN-SW.           
030414*    MOVE 'V'                    TO PY-VOID-SW.                   
01892 *    MOVE CH-CHECK-REFERENCE     TO PY-REF-NO.                    
01893                                                                   
01894      IF PI-AR-PROCESSING                                          
01895          MOVE 'B'                TO PY-PYMT-TYPE                  
01896          MOVE 'A'                TO PY-PMT-APPLIED.               
01897                                                                   
01898  3259-BUILD-EXIT.                                                 
01899      EXIT.                                                        
01900                                                                   
01901  3280-WRITE-ERPYAJ-REVERSAL.                                      
01902                                                                   
01903      EXEC CICS HANDLE CONDITION                                   
01904          DUPREC (3287-DUPREC)                                     
01905      END-EXEC.                                                    
01906                                                                   
01907      EXEC CICS WRITE                                              
01908          DATASET (ERPYAJ-FILE-ID)                                 
01909          FROM    (PENDING-PAY-ADJ)                                
01910          RIDFLD  (PY-CONTROL-PRIMARY)                             
01911      END-EXEC.                                                    
01912                                                                   
01913      GO TO 3289-WRITE-EXIT.                                       
01914                                                                   
01915  3287-DUPREC.                                                     
01916      ADD +1 TO PY-FILE-SEQ-NO.                                    
01917                                                                   
01918      GO TO 3280-WRITE-ERPYAJ-REVERSAL.                            
01919                                                                   
01920  3289-WRITE-EXIT.                                                 
01921      EXIT.                                                        
01922                                                                   
01923  3290-EXIT.                                                       
01924       EXIT.                                                       
01925                                                                   
01926      EJECT                                                        
01927  3300-UPDATE-ERPYAJ.                                              
01928                                                                   
01929      EXEC CICS GETMAIN                                            
01930          SET     (ADDRESS OF PENDING-PAY-ADJ)                     
01931          LENGTH  (200)                                            
01932          INITIMG (GETMAIN-SPACE)                                  
01933      END-EXEC.                                                    
01934                                                                   
01935      MOVE PI-COMPANY-CD          TO PYAJ-COMPANY-CD.              
01936      MOVE PI-CHEK-CARRIER        TO PYAJ-CARRIER.                 
01937      MOVE PI-CHEK-GROUPING       TO PYAJ-GROUPING.                
01938      MOVE PI-CR-FIN-RESP         TO PYAJ-FIN-RESP.                
01939      MOVE PI-CR-ACCOUNT          TO PYAJ-ACCOUNT.                 
01940      MOVE 'C'                    TO PYAJ-RECORD-TYPE.             
01941      MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO.             
01942                                                                   
01943      EXEC CICS HANDLE CONDITION                                   
111219         NOTFND   (3300-EXIT)                                     
111219         ENDFILE  (3300-EXIT)                                     
01946      END-EXEC.                                                    
01947                                                                   
111219 3300-READNEXT-ERPYAJ.                                            
01949                                                                   
01950      EXEC CICS READ                                               
01951          DATASET  (ERPYAJ-FILE-ID)                                
01952          SET      (ADDRESS OF PENDING-PAY-ADJ)                    
01953          RIDFLD   (ERPYAJ-KEY)                                    
01954          GTEQ                                                     
01955      END-EXEC.                                                    
01956                                                                   
01957      MOVE PY-CONTROL-PRIMARY TO ERPYAJ-KEY.                       
01958                                                                   
01959      IF (PI-COMPANY-CD    NOT = PY-COMPANY-CD) OR                 
01960         (PI-CHEK-CARRIER  NOT = PY-CARRIER)    OR                 
01961         (PI-CHEK-GROUPING NOT = PY-GROUPING)   OR                 
01962         (PI-CR-FIN-RESP   NOT = PY-FIN-RESP)   OR                 
01963         (PI-CR-ACCOUNT    NOT = PY-ACCOUNT)    OR                 
01964         (PY-RECORD-TYPE   NOT = 'C')                              
111219          GO TO 3300-EXIT.                                        
01966                                                                   
01967      IF WS-SV-AMOUNT-PAID NOT EQUAL PY-ENTRY-AMT                  
01968          ADD +1 TO PYAJ-FILE-SEQ-NO                               
111219         GO TO 3300-READNEXT-ERPYAJ.                              
01970                                                                   
01971      EXEC CICS READ UPDATE                                        
01972          DATASET   (ERPYAJ-FILE-ID)                               
01973          RIDFLD    (ERPYAJ-KEY)                                   
01974          SET       (ADDRESS OF PENDING-PAY-ADJ)                   
01975      END-EXEC.                                                    
01976                                                                   
01977      MOVE PAYTO1I                TO WS-TMS-PY-PAYEE.              
01978      MOVE CERTNOI                TO WS-TMS-PY-CERT.               
01979                                                                   
01980      MOVE WS-TMS-ENTRY-COMMENT TO PY-ENTRY-COMMENT.               
01981                                                                   
01982      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             
01983      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.         
01984      MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT              
01985                                                                   
01986      EXEC CICS REWRITE                                            
01987          DATASET  (ERPYAJ-FILE-ID)                                
01988          FROM     (PENDING-PAY-ADJ)                               
01989      END-EXEC.                                                    
01990                                                                   
111219 3300-EXIT.                                                       
01992       EXIT.                                                       
01993                                                                   
111219 3320-remove-rev-erpyaj-rec.
111219
111219     EXEC CICS GETMAIN                                            
111219        SET     (ADDRESS OF PENDING-PAY-ADJ)                     
111219        LENGTH  (200)                                            
111219        INITIMG (GETMAIN-SPACE)                                  
111219     END-EXEC
111219                                                                  
111219     MOVE PI-COMPANY-CD          TO erpyaj-key
111219     MOVE ch-comp-CARRIER        TO PYAJ-CARRIER
111219     MOVE ch-comp-grouping       TO PYAJ-GROUPING
111219     MOVE ch-comp-fin-resp       TO PYAJ-FIN-RESP
111219     MOVE ch-comp-account        TO PYAJ-ACCOUNT
111219     MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO
111219
111219     MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.                  
111219     MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.                   
111219     exec cics startbr
111219        dataset    ('ERPYAJ')
111219        ridfld     (erpyaj-key)
111219        resp       (ws-response)
111219     end-exec
111219
111219     if not resp-normal
111219        display ' no erpyaj found ' pyaj-carrier ' '
111219        pyaj-fin-resp ' ' pyaj-account
111219        go to 3320-exit
111219     end-if
111219
111219     set pyaj-browse-started to true
111219
111219     .
111219 3320-readnext.
111219
111219     exec cics readnext
111219        dataset     ('ERPYAJ')
111219        set         (address of pending-pay-adj)
111219        ridfld      (erpyaj-key)
111219        resp        (ws-response)
111219     end-exec
111219
111219     if not resp-normal
111219        display ' error 3320-readnext ' ws-response
111219        go to 3320-exit
111219     end-if
111219
111219     if ch-comp-carrier = py-carrier and
111219        ch-comp-fin-resp  = py-fin-resp and
111219        ch-comp-account = py-account
111219        if py-record-type = 'R' and
111219           ch-amount-paid = py-entry-amt and
111219           py-last-maint-dt = save-bin-date and
111219           py-gl-comment = 'VOID REFCK' and
111219           py-check-number = ch-check-no(2:6) and
111219           py-gl-account = '1825011300'
111219           exec cics delete
111219              dataset     ('ERPYAJ')
111219              ridfld      (erpyaj-key)
111219              resp        (ws-response)
111219           end-exec
111219           if not resp-normal
111219              display ' bad delete ' ws-response ' ' pyaj-account
111219           end-if
111219        else
111219           go to 3320-readnext
111219        end-if
111219     end-if
111219
111219     .
111219 3320-end-browse.
111219 
111219     if pyaj-browse-started
111219        exec cics endbr
111219            dataset    ('ERPYAJ')
111219        end-exec
111219     end-if
111219     .
111219 3320-exit.
111219     exit.

       3400-delete-record.

           EXEC CICS READ
               DATASET  (ERCHEK-FILE-ID)
               SET      (ADDRESS OF CHECK-RECORDS)
               RIDFLD   (PI-ERCHEK-KEY)
               UPDATE
               resp     (ws-response)
           END-EXEC

           if (ch-in-limbo or ch-approv-pending)
              and (ch-void-dt = low-values)
              and (ch-check-written-dt = low-values)
              and (ch-credit-accept-dt = low-values)
              perform 3500-check-tbl   thru 3500-exit
              if (row-deleted and tbl-commited)
      *          or (emi-error = er-3450)
                 exec cics delete
                    dataset   (erchek-file-id)
                 end-exec
                 move spaces to pi-check-cut
              end-if
           else
              move -1               to maintl
              move er-3452          to emi-error
              move al-uabon         to mainta
              perform 9900-error-format
                                       thru 9900-exit
              go to 8200-send-dataonly
           end-if

           if pi-chek-rec-cnt = +1
              PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT
              if resp-normal
                 MOVE ' '              TO PB-C-REFUND-SW
                 perform 6210-rewrite-pndb
                                       thru 6210-exit
              end-if
           end-if

           MOVE ER-0000            TO EMI-ERROR                     
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           MOVE LOW-VALUES             TO EL677AI.                      
                                                                        
           MOVE PI-CHEK-CARRIER        TO CARRIERO.                     
           MOVE PI-CHEK-GROUPING       TO GROUPO.                       
           MOVE PI-CHEK-STATE          TO STATEO.                       
           MOVE PI-CHEK-ACCOUNT        TO ACCTO.                        
                                                                        
           MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.                
           MOVE SPACE                  TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.                       
                                                                        
           MOVE PI-CHEK-CERT-NO        TO CERTNOO.                      
           MOVE PI-CHEK-SUF-NO         TO SFXO.                         
           MOVE PI-CHEK-SEQUENCE       TO SEQO.                         
                                                                        
           MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA      
                                          ACCTA   EFFDTA      
                                          CERTNOA   SFXA.               
                                                                        
           MOVE AL-UNNON               TO SEQA.                         
           GO TO 8100-SEND-INITIAL-MAP
           .
       3400-exit.
           exit.

       3500-check-tbl.

           perform 4100-CONNECT-TO-DB  thru 4100-exit
           if sqlcode = 0
              perform 4200-get-tbl-row thru 4200-exit
           end-if
            if sqlcode = 0
               if (nu-app-date = -1)
                  or (db-app-status = '2')
                  perform 4250-delete-row thru 4250-exit
               else
                 MOVE ER-3452            TO  EMI-ERROR                    
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
                 MOVE -1                 TO  PFENTERL                     
                 GO TO 8200-SEND-DATAONLY
              end-if
            else
00553          MOVE ER-3450            TO  EMI-ERROR                    
00554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00555          MOVE -1                 TO  PFENTERL                     
00556 *        GO TO 8200-SEND-DATAONLY
            end-if
           if connected-to-db
              perform 4300-FINISH-UP-DB thru 4300-exit
           end-if

           .
       3500-exit.
           exit.

02046  3900-RECORD-NOTFND.                                              
02047      MOVE ER-2908                TO EMI-ERROR.                    
02048      MOVE -1                     TO CARRIERL                      
02049      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA      
02050                                     ACCTA     EFFDTA  CERTNOA     
02051                                     SFXA      SEQA.               
02052      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02053      GO TO 8200-SEND-DATAONLY.                                    
02054      EJECT                                                        
02055  4000-ADD-CHANGE.                                                 

02056      IF PAYTO1L NOT = ZEROS                                      
02060         MOVE PAYTO1I             TO CH-PAYEE-NAME-1
           END-IF
02061                                                                   
02062      IF PAYTO2L NOT = ZEROS                                       
02063          MOVE PAYTO2I            TO CH-PAYEE-NAME-2.              
02064                                                                   
02065      IF PAYAD1L NOT = ZEROS                                       
02066          MOVE PAYAD1I            TO CH-PAYEE-ADDRESS-1.           
02067                                                                   
02068      IF PAYAD2L NOT = ZEROS                                       
02069          MOVE PAYAD2I            TO CH-PAYEE-ADDRESS-2.           
02070                                                                   

02071      IF PAYctyL NOT = ZEROS                                       
02072          MOVE payctyi            TO CH-PAYEE-CITY.
02073                                                                   
02071      IF PAYstL NOT = ZEROS                                       
02072          MOVE paysti            TO CH-PAYEE-state.
02073                                                                   
02074      IF PAYEEI GREATER SPACES                                     
02075         MOVE PAYEEI              TO CH-PAYEE-CODE.                
02076                                                                   
02077      IF PTOZIPL  =  ZEROS                                         
02078          MOVE ZEROS              TO CH-PAYEE-ZIP-CODE             
02079          GO TO 4000-CK-REST.                                      
02080                                                                   
02081      MOVE PTOZIPI                TO WS-ZIP-CODE.                  
02082                                                                   
02083      IF NOT WS-CANADIAN-POST-CODE                                 
02084          MOVE WS-ZIP-PRIME       TO CH-PAYEE-ZIP                  
02085          MOVE WS-ZIP-PLUS4       TO CH-PAYEE-ZIP-EXT              
02086          GO TO 4000-CK-REST.                                      
02087                                                                   
02088      MOVE SPACES                 TO CH-CANADIAN-POSTAL-CODE.      
02089      IF WS-CAN-POST-4TH = SPACE OR '-'                            
02090          MOVE WS-CAN-POST2-1     TO CH-CAN-POSTAL-1               
02091          MOVE WS-CAN-POST2-2     TO CH-CAN-POSTAL-2               
02092      ELSE                                                         
02093          MOVE WS-CAN-POST1-1     TO CH-CAN-POSTAL-1               
02094          MOVE WS-CAN-POST1-2     TO CH-CAN-POSTAL-2.              
02095                                                                   
02096  4000-CK-REST.                                                    
02097                                                                   
           IF RETTOL NOT = ZEROS
              MOVE RETTOI TO CH-RETURN-TO.

           if dedcynl not = zeros
              move dedcyni             to ch-deduct-commission
           end-if

      *    if dedcynl not = zeros
      *       if (dedcyni = 'Y')
      *          or (pi-prev-ded-comm = 'Y')
      *          compute ws-amt =
      *             pi-refund-on-pending-rec -
      *                pi-prev-paid - pi-ue-comm
      *       else
      *          if dedcyni = 'N'
      *             compute ws-amt =
      *                pi-refund-on-pending-rec -
      *                   pi-prev-paid-this-month
      *          end-if
      *       end-if
      *    end-if

091615     if pi-check-type = 'C'
091615        compute ws-amt =
091615           pi-endt-prm-diff - pi-prev-paid
091615        if (dedcyni = 'Y')
091615           or (pi-prev-ded-comm = 'Y')
091615           compute ws-amt =
091615              ws-amt - pi-ue-comm
091615        end-if
091615        if ws-amt <= zeros
091615           move pi-endt-prm-diff to ws-amt
091615        end-if
091615     end-if

02105      IF AMOUNTL NOT = ZEROS                                       
02106          MOVE WS-AMT             TO CH-AMOUNT-PAID                
02107          MOVE CH-AMOUNT-PAID     TO AMOUNTO.                      
02108                                                                   
02109      IF CHECKL NOT = ZEROS                                        
02110          MOVE CHECKI             TO CH-CHECK-NO.                  
02111                                                                   
02112      IF REASONL NOT = ZEROS                                       
02113          MOVE REASONI            TO CH-REASON-FOR-CHECK.          
02114                                                                   
02115 *    IF DEDAMTL NOT = ZEROS                                       
02116 *        MOVE WS-DED             TO CH-DEDUCT-WITHHELD            
02117 *                                   DEDAMTO.                      
02118                                                                   
02119 *    IF ADDLCHGL NOT = ZEROS                                      
02120 *        MOVE WS-ADDL            TO CH-ADDITIONAL-CHARGE          
02121 *                                   ADDLCHGO.                     
02122                                                                   
02123      IF STUBL NOT = ZEROS                                        
02124          MOVE STUBI              TO CH-STUB-LINE-1.               
02125                                                                   
02126      IF TEXT1L NOT = ZEROS                                        
02127          MOVE TEXT1I             TO CH-TEXT-LINE-1.               
02128                                                                   
02129      IF TEXT2L NOT = ZEROS                                        
02130          MOVE TEXT2I             TO CH-TEXT-LINE-2.               
02131                                                                   
02132      IF TEXT3L NOT = ZEROS                                        
02133          MOVE TEXT3I             TO CH-TEXT-LINE-3.               
02134                                                                   
02135      IF TYPEL NOT = ZEROS                                         
02136          MOVE TYPEI              TO CH-CHECK-ORIGIN-SW.           
02137                                                                   
02138 *    MOVE LETTER1I               TO CH-LETTERS (1).               
02139 *    MOVE LETTER2I               TO CH-LETTERS (2).               
02140 *    MOVE LETTER3I               TO CH-LETTERS (3).               
02141                                                                   
02142 *    MOVE SPACES                 TO CH-CHECK-REFERENCE.           
02143      IF CH-CHECK-ORIGIN-SW = 'R'                                  
02144          PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT       
02145 *        MOVE PI-REFERENCE           TO CH-CHECK-REFERENCE        
02146          MOVE PI-CANC-DT             TO CH-CANC-DT                
02147          MOVE PI-LF-REFUND           TO CH-LF-REFUND              
02148          MOVE PI-AH-REFUND           TO CH-AH-REFUND
091615     end-if

           MOVE PI-INSURED-NAME        TO CH-INSURED-NAME.          
02151 *    IF PI-AR-PROCESSING AND                                      
02152 *       REFL NOT = ZERO                                           
02153 *        MOVE REFI                   TO CH-CHECK-REFERENCE.       
02154                                                                   
02155  4000-EXIT.                                                       
02156       EXIT.                                                       

       4100-CONNECT-TO-DB.

030921     move 'HOVTSTDB01_ChkApprv'  to svr
030921     move 'appuser'              to usr
030921     move 'appuser@cso'          to pass
021714
021714     if ws-kix-myenv = 'cid1p'
030921        move 'SDVDB01_ChkApprv'  to svr
030921        move 'appuser'           to usr
030921        move 'appuser@cso'       to pass
021714     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
           end-if

           set connected-to-db to true

           .
       4100-EXIT.
           EXIT.

       4200-get-tbl-row.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  I'm only expecting one row so no cursor is declared       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           move pi-company-id          to ws-compid
           move ch-carrier             to ws-carrier
           move ch-grouping            to ws-grouping
           move ch-state               to ws-state
           move ch-account             to ws-account
           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-date
           end-if
           move ch-cert-prime          to ws-certificate
           move ch-cert-sfx            to ws-cert-sfx
           move ch-sequence-no         to ws-seq-no
           move '1'                    to ws-type
091615                                    ws-check-sub-type
091615     if pi-check-type = 'C'
091615        move '2'                 to ws-check-sub-type
091615     end-if

           exec sql
              SELECT
                 ApprovalStatus,
                 MaintainedBy,
                 MaintainedDate,
                 ApprovalBatch
              INTO 
                 :db-app-status :nu-app-status,
                 :db-app-by :nu-app-by,
                 :db-app-date :nu-app-date,
                 :db-app-batch :nu-app-batch
              FROM
                 ChkApp_Check
              WHERE
                 Company           = :ws-compid
                 and CertCarrier   = :ws-carrier
                 and CertGroup     = :ws-grouping
                 and CertState     = :ws-state
                 and CertAccount   = :ws-account
                 and CertEffDate   = :ws-eff-date
                 and CertNumber    = :ws-certificate
                 and CertNumberSuf = :ws-cert-sfx
                 and CheckSeqNbr   = :ws-seq-no
                 and CheckType     = :ws-type
091615           and CheckSubType  = :ws-check-sub-type
           end-exec

           if sqlcode not = 0
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4200-exit
           end-if

pemtst*    display ' status ' db-app-status
pemtst*    display ' by     ' db-app-by
pemtst*    display ' date   ' db-app-date
pemtst*    display ' batch  ' db-app-batch
             
pemtst*    if nu-app-date = -1
pemtst*       display ' approval date is low-values '
pemtst*    else
pemtst*       display ' approval date is NOT low values '
pemtst*    end-if

           .
       4200-exit.
           exit.

       4250-delete-row.

           EXEC SQL
              DELETE
                 from ChkApp_Check
              WHERE
                 Company           = :ws-compid
                 and CertCarrier   = :ws-carrier
                 and CertGroup     = :ws-grouping
                 and CertState     = :ws-state
                 and CertAccount   = :ws-account
                 and CertEffDate   = :ws-eff-date
                 and CertNumber    = :ws-certificate
                 and CertNumberSuf = :ws-cert-sfx
                 and CheckSeqNbr   = :ws-seq-no
                 and CheckType     = :ws-type
091615           and CheckSubType  = :ws-check-sub-type
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot delete row  "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4250-exit
           end-if

           set row-deleted         to true

           EXEC SQL
               commit transaction
           END-EXEC
           if sqlcode not = 0
              display "Error: commit "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4250-exit
           end-if
      
           set tbl-commited to true

           .
       4250-exit.
           exit.

       4300-FINISH-UP-DB.

           EXEC SQL
               commit work release
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       4300-EXIT.
           EXIT.

02160  5000-BROWSE-FILE.                                                

02161      EXEC CICS HANDLE CONDITION                                   
02162          NOTFND   (5560-END-OF-FILE)                              
02163          ENDFILE  (5560-END-OF-FILE)                              
02164      END-EXEC.                                                    
02165                                                                   
02166      MOVE PI-ERCHEK-KEY          TO ERCHEK-KEY.                   
02167                                                                   
02168      IF SEQI = LOW-VALUES                                         
02169          MOVE +1                 TO CHEK-RECORD-SEQ.              
02170                                                                   
02171      IF EIBAID = DFHPF2                                           
02172          GO TO 5100-BROWSE-BKWD.                                  
02173                                                                   
02174  5010-READ-LOOP.                                                  
02175      IF EIBAID = DFHPF1                                           
02176          ADD +1                  TO CHEK-RECORD-SEQ.              
02177                                                                   
02178      EXEC CICS READ                                               
02179          DATASET  (ERCHEK-FILE-ID)                                
02180          SET      (ADDRESS OF CHECK-RECORDS)                      
02181          RIDFLD   (ERCHEK-KEY)                                    
02182          GTEQ                                                     
02183      END-EXEC.                                                    
02184                                                                   
02185      IF CH-COMPANY-CD NOT = PI-COMPANY-CD                         
02186          IF EIBAID = DFHENTER                                     
02187              GO TO 5550-NO-RECORD                                 
02188          ELSE                                                     
02189              GO TO 5560-END-OF-FILE.                              
02190                                                                   

02192         IF PI-CHEK-CARRIER    = CH-CARRIER     AND                
02193            PI-CHEK-GROUPING   = CH-GROUPING    AND                
02194            PI-CHEK-STATE      = CH-STATE       AND                
02195            PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND                
02196            PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND                
02197            PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND                
02198            PI-CHEK-SUF-NO     = CH-CERT-SFX                       
02199               GO TO 5500-FORMAT-SCREEN                            
02200             ELSE                                                  
02201               IF EIBAID = DFHPF1                                  
02202                   GO TO 5560-END-OF-FILE                          
02203                 ELSE                                              
02204                   GO TO 5550-NO-RECORD.                           
02205                                                                   
02206      IF EIBAID = DFHENTER                                         
02207         IF CHEK-CARRIER    = CH-CARRIER     AND                   
02208            CHEK-GROUPING   = CH-GROUPING    AND                   
02209            CHEK-STATE      = CH-STATE       AND                   
02210            CHEK-ACCOUNT    = CH-ACCOUNT     AND                   
02211            CHEK-EFF-DT     = CH-CERT-EFF-DT AND                   
02212            CHEK-CERT-NO    = CH-CERT-PRIME  AND                   
02213            CHEK-SUF-NO     = CH-CERT-SFX    AND                   
02214            CHEK-RECORD-SEQ = CH-SEQUENCE-NO                       
02215               GO TO 5500-FORMAT-SCREEN                            
02216         ELSE                                                      
02217               GO TO 5550-NO-RECORD.                               
02218                                                                   
02219      GO TO 5500-FORMAT-SCREEN.                                    
02220                                                                   
02221      EJECT                                                        
02222                                                                   
02223  5100-BROWSE-BKWD.                                                
02224      EXEC CICS STARTBR                                            
02225          DATASET  (ERCHEK-FILE-ID)                                
02226          RIDFLD   (ERCHEK-KEY)                                    
02227      END-EXEC.                                                    
02228                                                                   
02229      EXEC CICS READPREV                                           
02230          DATASET  (ERCHEK-FILE-ID)                                
02231          SET      (ADDRESS OF CHECK-RECORDS)                      
02232          RIDFLD   (ERCHEK-KEY)                                    
02233      END-EXEC.                                                    
02234                                                                   
02235  5110-READ-LOOP.                                                  
02236      IF PI-FILE-EOF                                               
02237          MOVE SPACE              TO PI-EOF-SW                     
02238      ELSE                                                         
02239          EXEC CICS READPREV                                       
02240              DATASET  (ERCHEK-FILE-ID)                            
02241              SET      (ADDRESS OF CHECK-RECORDS)                  
02242              RIDFLD   (ERCHEK-KEY)                                
02243      END-EXEC.                                                    
02244                                                                   
02245      IF CH-COMPANY-CD NOT = PI-COMPANY-CD                         
02246          GO TO 5560-END-OF-FILE.                                  
02247                                                                   

02249         IF PI-CHEK-CARRIER    = CH-CARRIER     AND                
02250            PI-CHEK-GROUPING   = CH-GROUPING    AND                
02251            PI-CHEK-STATE      = CH-STATE       AND                
02252            PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND                
02253            PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND                
02254            PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND                
02255            PI-CHEK-SUF-NO     = CH-CERT-SFX                       
02256               GO TO 5500-FORMAT-SCREEN                            
02257             ELSE                                                  
02258               GO TO 5560-END-OF-FILE.                             
02259                                                                   
02260      GO TO 5500-FORMAT-SCREEN.                                    
02261                                                                   
02262      EJECT                                                        
02263                            COPY ELCNPD.                           
02264      EJECT                                                        
02265  5500-FORMAT-SCREEN.                                              

02266      MOVE LOW-VALUES             TO  EL677AI.                     
111418     if pi-void-reissue-pass = '1'
111418        move 'R'                 to mainti
111418                                    pi-prev-maint
111418     else
111418        MOVE 'S'                 TO MAINTI
111418                                    PI-PREV-MAINT
111418     end-if

02268      MOVE +1                     TO  MAINTL.                      
02269      MOVE CH-CONTROL-PRIMARY     TO  PI-ERCHEK-KEY.               
02270      MOVE CH-CARRIER             TO  CARRIERO.                    
02271      MOVE CH-GROUPING            TO  GROUPO.                      
02272      MOVE CH-STATE               TO  STATEO.                      
02273      MOVE CH-ACCOUNT             TO  ACCTO.                       
02274                                                                   
02275      MOVE CH-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02276      MOVE SPACE                  TO  DC-OPTION-CODE.              
02277      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
02278      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.                      
02279                                                                   
02280      MOVE CH-CERT-PRIME          TO  CERTNOO.                     
02281      MOVE CH-CERT-SFX            TO  SFXO.                        
02282      MOVE CH-SEQUENCE-NO         TO  SEQO.                        
02283                                                                   
02284      MOVE AL-UANON               TO  CARRIERA                     
02285                                      GROUPA                       
02286                                      STATEA                       
02287                                      ACCTA                        
02288                                      EFFDTA                       
02289                                      CERTNOA                      
02290                                      SFXA.                        
02291      MOVE CH-PAYEE-NAME-1         TO PI-PAYTO1.                   
02292                                                                   
           if ch-approval-dt not = low-values
              move ch-approval-dt      to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 8500-date-convert thru 8500-exit
              move dc-greg-date-1-edit to apvdto
           end-if
           evaluate ch-approval-status
              when '2'
                 move 'PENDING '       TO APVSTATO
              when 'P'
                 move 'PENDING '       TO APVSTATO
              when 'A'
                 move 'APPROVED'       to apvstato
              when 'D'
                 move 'DENIED'         to apvstato
           end-evaluate
           move ch-approved-by         to apvbyo

02293 *    IF PI-COMPANY-ID = 'TMS'                                     
02294 *        MOVE CH-PAYEE-NAME-2    TO  PAYTO1O                      
02295 *        MOVE CH-LIENHOLDER-NAME TO  PAYTO1AO                     
02296 *    ELSE                                                         
02297          MOVE CH-PAYEE-NAME-1    TO  PAYTO1O                     
02298          MOVE CH-PAYEE-NAME-2    TO  PAYTO2O.                     
02299                                                                   
02300      MOVE CH-PAYEE-ADDRESS-1     TO  PAYAD1O.                     
02301      MOVE CH-PAYEE-ADDRESS-2     TO  PAYAD2O.                     
           move ch-payee-city          to payctyo
           move ch-payee-state         to paysto

02303      MOVE CH-PAYEE-CODE          TO  PAYEEO.                      
02304                                                                   
02305      IF CH-PAYEE-ZIP-CODE NOT = ZEROS                             
02306          MOVE CH-PAYEE-ZIP-CODE  TO  PTOZIPO                      
02307          MOVE AL-UANON           TO  PTOZIPA.                     
02308                                                                   
           MOVE CH-RETURN-TO           TO  RETTOO

      *    if ch-check-cashed-dt not = spaces and low-values
      *       move ch-check-cashed-dt  to dc-bin-date-1
02310 *       MOVE SPACE               TO  DC-OPTION-CODE
02311 *       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02312 *       MOVE DC-GREG-DATE-1-EDIT TO  CASHEDO
      *    end-if

02309      MOVE CH-RECORDED-DT         TO  DC-BIN-DATE-1.               
02310      MOVE SPACE                  TO  DC-OPTION-CODE.              
02311      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
02312      MOVE DC-GREG-DATE-1-EDIT    TO  CREATEDO.                    
02313                                                                   
02314      MOVE CH-RECORDED-BY         TO  CBYO.                        
02315                                                                   
02321      IF CH-VOID-DT = SPACES OR LOW-VALUES OR ZEROS                
02322          NEXT SENTENCE                                            
02323      ELSE                                                         
02324          MOVE CH-VOID-DT             TO  DC-BIN-DATE-1            
02325          MOVE SPACE                  TO  DC-OPTION-CODE           
02326          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 
02327          MOVE DC-GREG-DATE-1-EDIT    TO  VOIDEDO                  
02328          MOVE CH-VOID-BY             TO  VBYO.                    
02329                                                                   
02330      IF CH-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES OR ZEROS       
02331          NEXT SENTENCE                                            
02332      ELSE                                                         
02333          MOVE CH-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1            
02334          MOVE SPACE                  TO  DC-OPTION-CODE           
02335          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 
02336          MOVE DC-GREG-DATE-1-EDIT    TO  PRINTEDI.                
02337                                                                   
02338      MOVE CH-AMOUNT-PAID         TO  AMOUNTO
111418                                     pi-void-reissue-amt
02339      MOVE AL-SANOF               TO  AMOUNTA.                     
02340      MOVE CH-CHECK-ORIGIN-SW     TO  TYPEI.                       
02341      MOVE AL-SANOF               TO  TYPEA.                       
02342      MOVE CH-CHECK-NO            TO  CHECKO.                      
           move ch-deduct-commission   to dedcyno
02343      MOVE CH-REASON-FOR-CHECK    TO  REASONO.                     
02344      MOVE CH-VOID-REASON         TO  VREASONO.                    
02345                                                                   
02350      MOVE CH-STUB-LINE-1         TO  STUBO.                      
02351      MOVE CH-TEXT-LINE-1         TO  TEXT1O.                      
02352      MOVE CH-TEXT-LINE-2         TO  TEXT2O.                      
02353      MOVE CH-TEXT-LINE-3         TO  TEXT3O.                      

02374      IF CH-LF-REFUND NOT NUMERIC                                  
02375          MOVE ZEROS              TO CH-LF-REFUND.                 
02376      IF CH-AH-REFUND NOT NUMERIC                                  
02377          MOVE ZEROS              TO CH-AH-REFUND.                 

           if (ch-check-no not = spaces and low-values)
pemtst        and (ch-check-written-dt not = low-values)
              perform 5800-fetch-check-cashed-dt
                                       thru 5800-exit
           end-if

02388      IF CH-CHECK-WRITTEN-DT GREATER LOW-VALUES OR                 
02389         CH-VOID-DT          GREATER LOW-VALUES                    
02390         MOVE AL-SANON            TO  CHECKA    REASONA  VREASONA  
02391                                      STUBA     TEXT1A   TEXT2A    
02392                                      TEXT3A    PAYTO1A  SFXA      
02393                                      PAYTO2A   PAYAD1A  PAYctyA   
                                           paysta    RETTOA   PAYAD2A
02394                                      PTOZIPA
02396                                      PRINTEDA                     
02397      ELSE                                                         
02398         MOVE AL-UANON            TO  CARRIERA  GROUPA   STATEA    
02399                                      ACCTA     EFFDTA   CERTNOA   
02400                                      CHECKA    REASONA  VREASONA  
02401                                      STUBA     TEXT1A   TEXT2A    
02402                                      TEXT3A    PAYTO1A  SFXA      
02403                                      PAYTO2A   PAYAD1A  PAYctyA   
                                           paysta    RETTOA   PAYAD2A
02405                                      PRINTEDA. 
02406                                                                   
02407      MOVE AL-UNNON               TO SEQA.                         

           perform 5600-get-erpndb     thru 5600-exit
           perform 5700-get-elcert     thru 5700-exit

091615*    move pi-prev-paid           to prepdo

           move al-sadof               to drefa

091615*    if WS-PNDB-FOUND
091615*       compute refo = pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
091615*    end-if

           if WS-CERT-FOUND
              compute ws-tot-lf-prem = cm-lf-premium-amt +
                 cm-lf-alt-premium-amt
              compute ws-tot-iss-prem  =
                 cm-ah-premium-amt + ws-tot-lf-prem
              compute ws-tot-iss-comm =
                 (ws-tot-lf-prem * cm-life-comm-pct) +
                 (cm-ah-premium-amt * cm-ah-comm-pct)
              compute ws-tot-ref-comm =
                 (pi-lf-refund * cm-life-comm-pct) +
                 (pi-ah-refund * cm-ah-comm-pct)
              move ws-tot-iss-prem     to premo
              move ws-tot-iss-comm     to isscommo
091615*       move ws-tot-ref-comm     to uecommo
           end-if

           if PI-TO-EL677-FROM-EL1273
              move zeros               to premo
                                          isscommo
                                          uecommo
                                          prepdo
                                          refo
           end-if

           GO TO 8100-SEND-INITIAL-MAP

           .
02413  5550-NO-RECORD.                                                  
02414      MOVE ER-1162                TO  EMI-ERROR.                   
02415      MOVE -1                     TO  CARRIERL.                    
02416      MOVE AL-UABON               TO  CARRIERA  GROUPA  STATEA     
02417                                      ACCTA     EFFDTA  CERTNOA    
02418                                      SFXA      SEQA.              
02419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02420                                                                   
02421      IF (NOT PI-TO-EL677-FROM-EL1273)
091615        and (pi-return-to-program not = 'EL6315')
02422          GO TO 8200-SEND-DATAONLY.                                
02423                                                                   
02424      MOVE CHEK-CARRIER           TO  CARRIERO.                    
02425      MOVE CHEK-GROUPING          TO  GROUPO.                      
02426      MOVE CHEK-STATE             TO  STATEO.                      
02427      MOVE CHEK-ACCOUNT           TO  ACCTO.                       
02428                                                                   
02429      MOVE CHEK-EFF-DT            TO  DC-BIN-DATE-1.               
02430      MOVE SPACE                  TO  DC-OPTION-CODE.              
02431      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
02432      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.                      
02433                                                                   
02434      MOVE CHEK-CERT-NO           TO  CERTNOO.                     
02435      MOVE CHEK-SUF-NO            TO  SFXO.                        
02436      GO TO 8100-SEND-INITIAL-MAP.                                 
02437                                                                   
02438  5560-END-OF-FILE.                                                
02439      IF EIBAID = DFHPF1                                           
02440          MOVE 'Y'                TO  PI-EOF-SW                    
02441          MOVE ER-2237            TO  EMI-ERROR                    
02442      ELSE                                                         
02443      IF EIBAID = DFHENTER                                         
02444         GO TO 5550-NO-RECORD                                      
02445      ELSE                                                         
02446 *        MOVE SPACES             TO  PI-ERCHEK-KEY                
02447          MOVE ER-2238            TO  EMI-ERROR.                   
02448                                                                   
02449      MOVE -1                     TO  MAINTL.                      
02450      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02451      GO TO 8200-SEND-DATAONLY.                                    
02452      EJECT                                                        

       5600-get-erpndb.

           MOVE 'N'                    TO WS-PNDB-FOUND-SW.             
                                                                        
           MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.        
           MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.           
           MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.          
           MOVE PI-STATE               TO ERPNDB-ALT-STATE.             
           MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.           
           MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.        
           MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.          
           MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.       
           MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.         
           MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.       
                                                                        
           IF ST-ACCNT-CNTL  OR                                         
              ACCNT-CNTL                                                
                MOVE SPACES             TO  ERPNDB-ALT-CARRIER.         
                                                                        
           IF ST-ACCNT-CNTL      OR                                     
              CARR-ST-ACCNT-CNTL OR                                     
              ACCNT-CNTL         OR                                     
              CARR-ACCNT-CNTL                                           
                MOVE SPACES             TO  ERPNDB-ALT-GROUPING.        
                                                                        
           IF ACCNT-CNTL OR                                             
              CARR-ACCNT-CNTL                                           
                MOVE SPACES             TO  ERPNDB-ALT-STATE.           
                                                                        
           EXEC CICS READ                                               
                DATASET   (ERPNDB-ALT-FILE-ID)                          
                SET       (ADDRESS OF PENDING-BUSINESS)                 
                RIDFLD    (ERPNDB-ALT-KEY)
                resp      (ws-response)
           END-EXEC

           if resp-normal
              MOVE 'Y'                 TO WS-PNDB-FOUND-SW
              compute pi-refund-on-pending-rec = 
                 pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
           end-if

           .
       5600-exit.
           exit.

       5700-get-elcert.

           MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.           
           MOVE PI-CARRIER             TO  ELCERT-CARRIER.              
           MOVE PI-GROUPING            TO  ELCERT-GROUPING.             
           MOVE PI-STATE               TO  ELCERT-STATE.                
           MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.              
           MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.           
           MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.             
           MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.          
                                                                        
           MOVE 'N'                    TO  WS-CERT-FOUND-SW.            

           EXEC CICS READ                                               
                DATASET   (ELCERT-FILE-ID)                              
                SET       (ADDRESS OF CERTIFICATE-MASTER)               
                RIDFLD    (ELCERT-KEY)
                resp      (ws-response)
           END-EXEC
                                                                        
           if resp-normal
              MOVE 'Y'                 TO WS-CERT-FOUND-SW
           end-if

           .
       5700-exit.
           exit.

       5800-fetch-check-cashed-dt.

           move ' '                    to ws-connect-sw
                                          ws-match-sw
030414                                    pi-check-cashed

           move 'NTCSO2_PdBnkInfo'     to svr
           move 'sa'                   to usr
           move 'ntcso2'               to pass

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to pdbnkinfo "
              display sqlcode
              display sqlerrmc
              go to 5800-exit
           end-if

           set connected-to-db to true

           if pi-company-id = 'CID'
              move 'CSO - AP%'         to ws-pb-compid
           else
              move 'AHL - AP%'         to ws-pb-compid
           end-if

           move '000'                  to ws-pb-check-no (1:3)
           move ch-check-no            to ws-pb-check-no (4:7)

           move spaces                 to ws-pb-bad-check-no
           perform varying s1 from +1 by +1 until
              (s1 > +10)
              or (ws-pb-check-no (s1:1) not = '0')
           end-perform

           if s1 < +11
              move ws-pb-check-no (s1:11 - s1)
                                  to ws-pb-bad-check-no (1: 11 - s1)
           end-if

      *    display ' compid **' ws-pb-compid '**'
      *    display ' ckno  **' ws-pb-check-no '**'


021714     MOVE ch-AMOUNT-PAID      TO WS-CHECK-AMT-TMP
021714     MOVE WS-CHECK-AMT-TMPX   TO WS-CHECK-AMOUNT
021714     MOVE SPACES              TO pb-paid-date
021714
021714     EXEC SQL
021714        CALL pbi_GetCashDate_by_TransactionNbr
021714           @compid          = :ws-pb-compid,
021714           @checkno         = :ws-pb-check-no,
021714           @badcheckno      = :ws-pb-bad-check-no,
021714           @checkamount     = :ws-check-amount,
111219           @checkcasheddate = :pb-paid-date :nu-app-date OUT
021714     END-EXEC

           if sqlcode not = 0
              display "Error: cannot run stor proc  "
              move sqlcode            to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 5800-disconnect
           end-if

111219     if nu-app-date = -1 *> no check cashed date
111219        go to 5800-disconnect
111219     end-if

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The following code works just as well as the above        ***
      ***  stored procedure and just as fast.                        ***
      ***  Originally, I had declared a cursor and fetched through   ***
      ***  the record set to find the correct check and that REALLY  ***
      ***  slowed the response down to ~ 3/4 second.                 ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

      *     EXEC SQL
      *        SELECT
      *           PaidDate
      *        INTO 
      *           :pb-paid-date
      *        FROM
      *           dbo.cso_pbi_TransactionLookup
      *        WHERE
      *           (BankAcctDescr like :ws-pb-compid)
      *           and (Amount = :ws-check-amount)
      *           and ((transactionNbr = :ws-pb-check-no)
      *                  or
      *               (transactionnbr = :ws-pb-bad-check-no))
      **          and (CAST(TransactionNbr AS INT) =
      **             :ws-pb-check-no-num)
      **          and (right('00000' + rtrim(TransactionNbr),10)
      **             = :ws-pb-check-no)
      **          and (TransactionNbr  = :ws-pb-check-no)
      *     END-EXEC

pemtst*    display ' chkno  ' pb-check-no
pemtst*    display ' desc   ' pb-bank-acct-desc
pemtst*    display ' trn typ' pb-tran-type
pemtst*    display ' amt   *' pb-amount '**'
pemtst*    display ' pd dte ' pb-paid-date

           string pb-paid-date (1:4)
                  pb-paid-date (6:2)
                  pb-paid-date (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
      *       move dc-bin-date-1       to ws-bin-cashed-dt
              move dc-greg-date-1-edit to cashedo
030414        move 'Y' to pi-check-cashed
           else
              display ' error cvtdte cash dt ' pb-paid-date ' '
                 dc-error-code
           end-if

           .
       5800-disconnect.

           EXEC SQL
              DISCONNECT ALL
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot disconnect pdbnk "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       5800-exit.
           exit.

02454  6000-VERIFY-COMP-MASTER.                                         
02455      EXEC CICS HANDLE CONDITION                                   
02456          NOTFND   (6070-NO-COMP-MSTR)                             
02457      END-EXEC.                                                    
02458                                                                   
02459      MOVE PI-COMPANY-CD          TO  ERCOMP-COMP-CD.              
02460                                                                   
02461      IF NOT PI-ZERO-CARRIER AND                                   
02462         NOT PI-ZERO-CAR-GROUP                                     
02463          MOVE PI-CR-CARRIER      TO  ERCOMP-CARRIER               
02464      ELSE                                                         
02465          MOVE ZEROS              TO  ERCOMP-CARRIER.              
02466                                                                   
02467      IF NOT PI-ZERO-GROUPING  AND                                 
02468         NOT PI-ZERO-CAR-GROUP                                     
02469          MOVE PI-CR-GROUPING     TO  ERCOMP-GROUPING              
02470      ELSE                                                         
02471          MOVE ZEROS              TO  ERCOMP-GROUPING.             
02472                                                                   
02473      MOVE PI-CR-FIN-RESP         TO  ERCOMP-FIN-RESP.             
02474      MOVE PI-CR-ACCOUNT          TO  ERCOMP-ACCOUNT.              
02475      MOVE 'A'                    TO  ERCOMP-RECORD-TYPE.          
02476                                                                   
02477      EXEC CICS READ                                               
02478           DATASET   (ERCOMP-FILE-ID)                              
02479           SET       (ADDRESS OF COMPENSATION-MASTER)              
02480           RIDFLD    (ERCOMP-KEY)                                  
02481       END-EXEC.                                                   
02482                                                                   
02483      GO TO 6090-EXIT.                                             
02484                                                                   
02485  6070-NO-COMP-MSTR.                                               
02486      MOVE -1                     TO  CARRIERL.                    
02487      MOVE ER-2230                TO  EMI-ERROR.                   
02488      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02489      GO TO 8200-SEND-DATAONLY.                                    
02490                                                                   
02491  6090-EXIT.                                                       
02492       EXIT.                                                       
02493      EJECT                                                        
02494  6100-VERIFY-CERTIFICATE.                                         

02495      EXEC CICS HANDLE CONDITION                                   
02496          NOTFND   (6170-NO-CERTIFICATE)                           
02497      END-EXEC.                                                    
02498                                                                   
02499      MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.           
02500      MOVE PI-CARRIER             TO  ELCERT-CARRIER.              
02501      MOVE PI-GROUPING            TO  ELCERT-GROUPING.             
02502      MOVE PI-STATE               TO  ELCERT-STATE.                
02503      MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.              
02504      MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.           
02505      MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.             
02506      MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.          
02507                                                                   
02508      MOVE 'N'                    TO  WS-CERT-FOUND-SW.            
02509                                                                   
02510      MOVE SPACES                 TO  PI-REFERENCE                 
02511 *                                    PI-INSURED-NAME.             
02512      MOVE LOW-VALUES             TO  PI-CANC-DT.                  
02513 *    MOVE ZEROS                  TO  PI-LF-REFUND                 
02514 *                                    PI-AH-REFUND.                
02515                                                                   
02516      EXEC CICS READ                                               
02517           DATASET   (ELCERT-FILE-ID)                              
02518           SET       (ADDRESS OF CERTIFICATE-MASTER)               
02519           RIDFLD    (ELCERT-KEY)                                  
02520       END-EXEC.                                                   
02521                                                                   
02522      MOVE 'Y'                    TO  WS-CERT-FOUND-SW.            
02523                                                                   
02524 *    MOVE CM-LF-ITD-CANCEL-AMT   TO  PI-LF-REFUND.                
02525 *    MOVE CM-AH-ITD-CANCEL-AMT   TO  PI-AH-REFUND.                
02526      IF CM-LF-CANCEL-DT NOT = LOW-VALUES                          
02527          MOVE CM-LF-CANCEL-DT    TO  PI-CANC-DT                   
02528      ELSE                                                         
02529          IF CM-AH-CANCEL-DT NOT = LOW-VALUES                      
02530              MOVE CM-AH-CANCEL-DT TO PI-CANC-DT.                  
02531 *    MOVE CM-INSURED-LAST-NAME   TO  PI-INSURED-NAME.             
02532                                                                   
02533      GO TO 6190-EXIT.                                             
02534                                                                   
02535  6170-NO-CERTIFICATE.                                             
02536      MOVE -1                     TO  CARRIERL.                    
02537      MOVE ER-2726                TO  EMI-ERROR.                   
02538      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02539                                                                   
02540  6190-EXIT.                                                       
02541       EXIT.                                                       
02542                                                                   
02543      EJECT                                                        
02544  6200-UPDATE-PENDING-BUS-REC.                                     

02549      MOVE PI-COMPANY-CD          TO  ERPNDB-ALT-COMPANY-CD.       
02550      MOVE PI-CARRIER             TO  ERPNDB-ALT-CARRIER.          
02551      MOVE PI-GROUPING            TO  ERPNDB-ALT-GROUPING.         
02552      MOVE PI-STATE               TO  ERPNDB-ALT-STATE.            
02553      MOVE PI-ACCOUNT             TO  ERPNDB-ALT-ACCOUNT.          
02554      MOVE PI-CERT-PRIME          TO  ERPNDB-ALT-CERT-PRIME.       
02555      MOVE PI-CERT-SFX            TO  ERPNDB-ALT-CERT-SFX.         
02556      MOVE PI-CERT-EFF-DT         TO  ERPNDB-ALT-CERT-EFF-DT.      
02557      MOVE ZEROS                  TO  ERPNDB-ALT-CH-SEQ-NO.        
02558      MOVE '2'                    TO  ERPNDB-ALT-RECORD-TYPE.      
02559                                                                   
02560      IF ST-ACCNT-CNTL  OR                                         
02561         ACCNT-CNTL                                                
02562           MOVE SPACES             TO  ERPNDB-ALT-CARRIER.         
02563                                                                   
02564      IF ST-ACCNT-CNTL      OR                                     
02565         CARR-ST-ACCNT-CNTL OR                                     
02566         ACCNT-CNTL         OR                                     
02567         CARR-ACCNT-CNTL                                           
02568           MOVE SPACES             TO  ERPNDB-ALT-GROUPING.        
02569                                                                   
02570      IF ACCNT-CNTL OR                                             
02571         CARR-ACCNT-CNTL                                           
02572           MOVE SPACES             TO  ERPNDB-ALT-STATE.           
02573                                                                   
02574      EXEC CICS READ                                               
02575           DATASET   (ERPNDB-ALT-FILE-ID)                          
02576           SET       (ADDRESS OF PENDING-BUSINESS)                 
02577           RIDFLD    (ERPNDB-ALT-KEY)                              
                resp      (ws-response)
02578       END-EXEC.                                                   
02579                                                                   
           if not resp-normal
              go to 6200-exit
           end-if

02580      MOVE PB-CONTROL-PRIMARY     TO  ERPNDB-PRIMARY-KEY.          
02581                                                                   
02582      EXEC CICS READ                                               
02583           DATASET   (ERPNDB-FILE-ID)                              
02584           SET       (ADDRESS OF PENDING-BUSINESS)                 
02585           RIDFLD    (ERPNDB-PRIMARY-KEY)                          
02586           UPDATE                                                  
                resp      (ws-response)
02587       END-EXEC.                                                   
02600                                                                   
02601  6200-EXIT.                                                       
02602       EXIT.                                                       

       6210-rewrite-pndb.

02596      EXEC CICS REWRITE                                            
02597          DATASET  (ERPNDB-FILE-ID)                                
02598          FROM     (PENDING-BUSINESS)                              
02599      END-EXEC

           .
       6210-exit.
           exit.

02605  6300-BUILD-CERT-SCREEN.                                          

      *    move 'N'                    to dedcyni
      *                                   pi-prev-ded-comm
           MOVE LOW-VALUES             TO  EL677AI.                     
           perform 6700-build-common   thru 6700-exit

           if ws-refund-amount = zeros
              move pi-company-cd       to pi-chek-comp-cd
              move pi-carrier          to pi-chek-carrier
              move pi-grouping         to pi-chek-grouping
              move pi-state            to pi-chek-state
              move pi-account          to pi-chek-account
              move pi-cert-eff-dt      to pi-chek-eff-dt
              move pi-cert-prime       to pi-chek-cert-no
              move pi-cert-sfx         to pi-chek-suf-no
              move +1                  to pi-chek-sequence

00494              MOVE 'S'            TO MAINTI                        
00495              MOVE 1              TO MAINTL                        
00496              MOVE AL-UABON       TO MAINTA                        
00497              MOVE DFHENTER       TO EIBAID                        


              go to 5000-browse-file
           end-if

02643      IF WS-CERT-NOT-FOUND                                         
02644          GO TO 8100-SEND-INITIAL-MAP.                             
02645                                                                   
02646 *    IF WS-PNDB-FOUND                                             
02647 *        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +          
02648 *                                   PB-C-AH-CANCEL-AMT            
02649 *        MOVE 'R'                TO  TYPEO                        
02650 *        IF PI-AR-PROCESSING                                      
02651 *            MOVE AL-UANON       TO  REFA                         
02652 *            MOVE PB-C-REFERENCE TO  REFO                         
02653 *        END-IF                                                   
02654 *    ELSE                                                         
02655 *        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +        
02656 *                                   CM-AH-ITD-CANCEL-AMT.         

02658      MOVE WS-REFUND-AMOUNT        TO AMOUNTO
091615     MOVE pi-check-type           TO TYPEO
02659                                                                   
02673      IF PI-COMPANY-ID = 'TMS'                                     
02674          MOVE WS-NAME-WORK         TO PAYTO1I                     
02675                                       PI-PAYTO1                   
02676          MOVE AL-UANON             TO PAYTO1A                     
02677      ELSE                                                         
02678          MOVE WS-NAME-WORK         TO PAYTO1I                    
02679                                       PI-PAYTO1                   
02680          MOVE AL-UANON             TO PAYTO1A.                   
02681                                                                   
02682 *    MOVE CM-BENEFICIARY           TO PAYTO1AI.                   
02683 *    MOVE AL-UANON                 TO PAYTO1AA.                   

           move 'N'                    to dedcyni
           move al-uanon               to dedcyna
           move 'INS'                  to payeeo
           move al-panon               to payeea

02685      IF PI-MAIL-YES                                               
02686          PERFORM 6400-MAILING-ADDRESS THRU 6490-EXIT.             
02687                                                                   
02688      GO TO 8100-SEND-INITIAL-MAP.                                 
02689                                                                   
02690      EJECT                                                        
02691                                                                   
02692  6400-MAILING-ADDRESS.                                            
02693      EXEC CICS HANDLE CONDITION                                   
02694          NOTFND   (6470-NO-ADDRESS)                               
02695      END-EXEC.                                                    
02696                                                                   
02697      EXEC CICS READ                                               
02698          DATASET   (ERMAIL-FILE-ID)                               
02699          SET       (ADDRESS OF MAILING-DATA)                      
02700          RIDFLD    (ELCERT-KEY)                                   
02701      END-EXEC.                                                    
02702                                                                   
02703      MOVE MA-ADDRESS-LINE-1      TO PAYAD1I.                      
02704      MOVE MA-ADDRESS-LINE-2      TO PAYAD2I.                      
           move ma-city                to payctyi
           move ma-addr-state          to paysti

02706      MOVE MA-ZIP                 TO PTOZIPI.                      
02707      MOVE AL-UANON               TO PAYAD1A                       
02708                                     PAYAD2A                       
02709                                     PAYctyA                       
                                          paysta
02710                                     PTOZIPA.                      
02711                                                                   
02712      GO TO 6490-EXIT.                                             
02713                                                                   
02714  6470-NO-ADDRESS.                                                 
02715      MOVE -1                     TO  PAYAD1L.                     
02716      MOVE ER-2394                TO  EMI-ERROR.                   
02717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02718                                                                   
02719  6490-EXIT.                                                       
02720       EXIT.                                                       
02721                                                                   
02722      EJECT                                                        
02723                                                                   
02724  6500-BUILD-ACCOUNT-SCREEN.                                       

           MOVE LOW-VALUES             TO  EL677AI.                     
           perform 6700-build-common   thru 6700-exit

02658      MOVE WS-REFUND-AMOUNT       TO AMOUNTO
02756      MOVE PI-AM-NAME             TO PAYTO1I
           MOVE SPACES                 TO PAYTO2I
02757      MOVE PI-AM-ADDRS            TO PAYAD1I.                      
02758      MOVE PI-AM-CITY             TO PAYCTYI
           move pi-am-st               to paysti
02759      MOVE PI-AM-ZIP-CODE         TO PTOZIPI
           move 'ACCT'                 to payeeo
           move al-panon               to payeea

02761      MOVE AL-UANON               TO PAYTO1A                       
02762                                     PAYTO2A                       
02763                                     PAYAD1A
                                          PAYCTYA
                                          paysta
02764                                     PTOZIPA

02770      GO TO 8100-SEND-INITIAL-MAP

           .
02774  6600-BUILD-BENEFICIARY-SCREEN.                                   

           MOVE LOW-VALUES             TO  EL677AI.                     
           perform 6700-build-common   thru 6700-exit

02643      IF WS-CERT-NOT-FOUND                                         
02644          GO TO 8100-SEND-INITIAL-MAP.                             
02645                                                                   
02646 *    IF WS-PNDB-FOUND                                             
02647 *        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +          
02648 *                                   PB-C-AH-CANCEL-AMT            
02649 *        MOVE 'R'                TO  TYPEO                        
02650 *        IF PI-AR-PROCESSING                                      
02651 *            MOVE AL-UANON       TO  REFA                         
02652 *            MOVE PB-C-REFERENCE TO  REFO                         
02653 *        END-IF                                                   
02654 *    ELSE                                                         
02655 *        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +        
02656 *                                   CM-AH-ITD-CANCEL-AMT.         
02657                                                                   
02658      MOVE WS-REFUND-AMOUNT        TO  AMOUNTO.                    
091615     MOVE pi-check-type           TO  TYPEO.                      
02659                                                                   
02673      IF PI-COMPANY-ID = 'TMS'                                     
02674          MOVE WS-NAME-WORK         TO PAYTO1I                     
02675                                       PI-PAYTO1                   
02676          MOVE AL-UANON             TO PAYTO1A                     
02677      ELSE                                                         
02678          MOVE WS-NAME-WORK         TO PAYTO1I                    
02679                                       PI-PAYTO1                   
02680          MOVE AL-UANON             TO PAYTO1A.                   
02681                                                                   
02682 *    MOVE CM-BENEFICIARY           TO PAYTO1AI.                   
02683 *    MOVE AL-UANON                 TO PAYTO1AA.                   
02684                                                                   
           move 'N'                    to dedcyni
           move al-uanon               to dedcyna
           move 'BENE'                 to payeeo
           move al-panon               to payeea
02685      IF PI-MAIL-YES                                               
02686          PERFORM 6620-MAILING-ADDRESS THRU 6690-EXIT.             
02687                                                                   
02688      GO TO 8100-SEND-INITIAL-MAP.                                 
02689                                                                   
02690      EJECT                                                        
02691                                                                   
02692  6620-MAILING-ADDRESS.                                            
02693      EXEC CICS HANDLE CONDITION                                   
02694          NOTFND   (6670-NO-beneficiary)                           
02695      END-EXEC.                                                    
02696                                                                   
02697      EXEC CICS READ                                               
02698          DATASET   (ERMAIL-FILE-ID)                               
02699          SET       (ADDRESS OF MAILING-DATA)                      
02700          RIDFLD    (ELCERT-KEY)                                   
02701      END-EXEC.                                                    
02702                                                                   
           move spaces                 to payto2i
           move ma-cred-bene-name      to payto1i
02703      MOVE MA-cred-bene-addr      TO PAYad1I.                      
02704      MOVE MA-cred-bene-addr2     TO PAYad2I.                      
           move ma-cred-bene-city      to payctyi
           move ma-cred-bene-state     to paysti

02706      MOVE MA-cred-bene-zip       TO PTOZIPI.                      
02707      MOVE AL-UANON               TO payto1a
                                          PAYTO2A                       
02708                                     PAYad1A
                                          payad2a
02709                                     PAYctyA                       
                                          paysta
02710                                     PTOZIPA.                      
02711                                                                   
02712      GO TO 6690-EXIT.                                             
02713                                                                   
02835  6670-NO-BENEFICIARY.                                             

02715      MOVE -1                     TO  PAYTO1L.                     
02716      MOVE ER-2394                TO  EMI-ERROR.                   
02717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02718                                                                   
02719  6690-EXIT.                                                       
02720       EXIT.                                                       

       6700-build-common.

           if not eracct-found
              move pi-company-cd       to eracct-co
              move pi-carrier          to eracct-carrier
              move pi-grouping         to eracct-grouping
              move pi-state            to eracct-state
              move pi-account          to eracct-account
              move pi-cert-eff-dt      to eracct-exp-date
              perform 1420-get-eracct  thru 1490-exit
           end-if
      *    if WS-PNDB-NOT-FOUND
      *       perform 5600-get-erpndb thru 5600-exit
      *    end-if

      *    MOVE LOW-VALUES             TO  EL677AI.                     
           MOVE 'A'                    TO  MAINTI.                      
           MOVE AL-UANON               TO  MAINTA.                      
           MOVE +1                     TO  MAINTL.                      
                                                                        
           MOVE PI-CARRIER             TO  CARRIERO.                    
           MOVE PI-GROUPING            TO  GROUPO.                      
           MOVE PI-STATE               TO  STATEO.                      
           MOVE PI-ACCOUNT             TO  ACCTO.                       
           MOVE PI-CERT-PRIME          TO  CERTNOO.                     
           MOVE PI-CERT-SFX            TO  SFXO.                        
           MOVE PI-AMOUNT              TO  AMOUNTO.                     
091615*    MOVE PI-TYPE                TO  TYPEO.                       
091615     move pi-check-type          to  typeo
           move pi-return-to           to  rettoo
      *    MOVE PI-REFERENCE           TO  REFO.                        
           MOVE PI-PAYTO1              TO  PAYTO1O.                    
           MOVE AL-UANON               TO  CARRIERA                     
                                           GROUPA                       
                                           STATEA                       
                                           ACCTA                        
                                           CERTNOA                      
                                           SFXA                         
                                           PAYTO1A                      
                                           PAYTO2A                     
      *                                    AMOUNTA                      
091615*                                    TYPEA
                                           rettoa
                                           payeea.                       
091615     move al-panon               to typea
           move al-panon               to amounta
           MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
           MOVE ' '                    TO  DC-OPTION-CODE.              
           PERFORM 8500-DATE-CONVERT.                                   
                                                                        
           IF NO-CONVERSION-ERROR                                       
               MOVE DC-GREG-DATE-1-EDIT TO  EFFDTI                      
               MOVE AL-UANON            TO  EFFDTA                      
               MOVE 'Y'                 TO  PI-PROCESS-CERT-SW          
               PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT           
           ELSE                                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           IF CM-INSURED-FIRST-NAME GREATER SPACES                      
               MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME      
               MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT      
           ELSE                                                         
               MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME      
               MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.     
                                                                        
           MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.    
                                                                        
           PERFORM 5200-MOVE-NAME THRU 5200-EXIT.                       
                                                                        

           move spaces to pi-insured-name
           string cm-insured-last-name ' '
                  cm-insured-first-name delimited by '  '
              into pi-insured-name
           end-string

           move spaces                 to pi-table-name
           string cm-insured-last-name ', '
                  cm-insured-first-name ' '
                  cm-insured-initial2
              delimited by '  '
              into
                 pi-table-name
           end-string
      *    MOVE WS-NAME-WORK           TO PI-INSURED-NAME.            
                                                                        
           if eibaid = dfhpf5
              move 'Y'                 to dedcyni
              move al-uanon            to dedcyna
           end-if
     
          compute ws-tot-lf-prem = cm-lf-premium-amt +
             cm-lf-alt-premium-amt
          compute ws-tot-iss-prem  =
             cm-ah-premium-amt + ws-tot-lf-prem
          compute ws-tot-iss-comm =
             (ws-tot-lf-prem * cm-life-comm-pct) +
             (cm-ah-premium-amt * cm-ah-comm-pct)
          compute ws-tot-ref-comm =
             (pi-lf-refund * cm-life-comm-pct) +
             (pi-ah-refund * cm-ah-comm-pct)

           move ws-tot-iss-prem        to premo
           move ws-tot-iss-comm        to isscommo
091615     move al-sadof               to drefa
091615                                    refa
091615*    compute refo = pi-lf-refund + pi-ah-refund
           move pi-prev-paid            to prepdo
           move ws-tot-ref-comm         to uecommo
                                           pi-ue-comm

      ***  compute ws-refund-amount = pi-refund-on-pending-rec -
      ***     pi-prev-paid
101216     compute ws-refund-amount =
103116        pi-refund-on-pending-rec - pi-prev-paid-this-month

           if (pi-refund-on-pending-rec + pi-prev-paid) >
              ws-tot-iss-prem
              move zeros               to ws-refund-amount
           end-if

           if ((dedcyni = 'Y')
              or (pi-prev-ded-comm = 'Y'))
              and (ws-refund-amount >= ws-tot-ref-comm)
              compute ws-refund-amount = ws-refund-amount -
                 ws-tot-ref-comm
           end-if

091615     if pi-check-type = 'C'
091615*       move pi-endt-prm-diff    to refo
091615        move pi-endt-com-diff    to uecommo
091615                                    pi-ue-comm
091615        compute ws-refund-amount =
091615           pi-endt-prm-diff - pi-prev-paid
091615        if (dedcyni = 'Y')
091615           or (pi-prev-ded-comm = 'Y')
091615           compute ws-refund-amount = ws-refund-amount -
091615              pi-endt-com-diff
091615        end-if
091615        if ws-refund-amount <= zeros 
091615           move pi-endt-prm-diff to ws-refund-amount
091615        end-if
091615     end-if

           if not WS-CERT-NOT-FOUND
              string
                 pi-cert-prime         ' : '
                 cm-insured-last-name  ', '
                 cm-insured-first-name ' : '
                 pi-account            ' : '
                 pi-am-csr delimited by '  ' into text1o
              end-string
              move al-uanon            to text1a
              MOVE 'REF'               TO WS-USER-REASON
091615        if pi-check-type = 'C'
091615           move 'COR'            to ws-user-reason
091615        end-if
              MOVE pi-cert-no          TO WS-USER-CERT-NO
              inspect ws-user-cert-no replacing leading zeros
                 by spaces
              move pi-insured-name     to ws-user-name
              
              perform varying s1 from +1 by +1 until
                 (s1 > +11)
                 or (ws-user-cert-no (s1:1)) <> ' '
              end-perform
              string ws-user-reason  ' ' delimited by size
                     ws-user-cert-no (s1:11 - s1 + 1) ' '
                        delimited by size
                     cm-insured-last-name ' '
                     cm-insured-first-name delimited by '  '
                 into stubo
              end-string
              move al-uanon            to stuba
           end-if

           .
       6700-exit.
           exit.

       6800-browse-previous-chek-recs.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      **** this should be the first time through, we need to gather  ***
      **** all the erchek records for this pending record, add the   ***
      **** amounts and make sure this check request will not exceed  ***
      **** the collected premium                                     ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           move zeros                  to pi-prev-paid
103116                                    pi-prev-paid-this-month
                                          ws-browse-sw
                                          pi-chek-rec-cnt
           move spaces                 to pi-prev-ded-comm
           move pi-company-cd          to chek-company-cd
           move pi-carrier             to chek-carrier
           move pi-grouping            to chek-grouping
           move pi-state               to chek-state
           move pi-account             to chek-account
           move pi-cert-prime          to chek-cert-no
           move pi-cert-sfx            to chek-suf-no
           move pi-cert-eff-dt         to chek-eff-dt
           move +0                     to chek-record-seq
           move erchek-key             to ws-compare-erchek-key

           EXEC CICS STARTBR
              DATASET  (ERCHEK-FILE-ID)
              RIDFLD   (ERCHEK-KEY)
              resp     (ws-response)
           END-EXEC
     
           if not resp-normal
              go to 6800-exit
           end-if

           perform until i-say-when
              EXEC CICS READNEXT
                 DATASET   (ERCHEK-FILE-ID)
                 SET       (ADDRESS OF CHECK-RECORDS)
                 RIDFLD    (ERCHEK-KEY)
                 resp      (ws-response)
              END-EXEC
              if resp-normal
                 and erchek-key (1:33) = ws-compare-erchek-key
                 if (ch-void-dt = low-values)
                    and (not ch-denied)
091615              if pi-check-type = 'R'
091615                 and ch-check-origin-sw = 'C'
091615                 continue
091615              else
091615                 compute pi-prev-paid = pi-prev-paid +
091615                    ch-amount-paid
091615                 add +1 to pi-chek-rec-cnt
091615                 move ch-deduct-commission
091615                                 to pi-prev-ded-comm
103116                 if ch-credit-select-dt = pi-cr-month-end-dt
103116                    compute pi-prev-paid-this-month =
103116                       pi-prev-paid-this-month + ch-amount-paid
                       end-if
091615              end-if
                 end-if
              else
                 set i-say-when to true
              end-if
           end-perform

           .
       6800-exit.
           exit.

02846  7000-SET-PI-AREA.                                                
02847      MOVE CARRIERI               TO  PI-CARRIER                   
02848                                      PI-CHEK-CARRIER.             
02849      MOVE GROUPI                 TO  PI-GROUPING                  
02850                                      PI-CHEK-GROUPING.            
02851      MOVE STATEI                 TO  PI-STATE                     
02852                                      PI-CHEK-STATE.               
02853      MOVE ACCTI                  TO  PI-ACCOUNT                   
02854                                      PI-CHEK-ACCOUNT.             
02855      MOVE CERTNOI                TO  PI-CERT-PRIME                
02856                                      PI-CHEK-CERT-NO.             
02857      IF SFXI NOT = LOW-VALUES                                     
02858          MOVE SFXI               TO  PI-CERT-SFX                  
02859                                      PI-CHEK-SUF-NO               
02860      ELSE                                                         
02861          MOVE SPACE              TO  PI-CERT-SFX                  
02862                                      PI-CHEK-SUF-NO.              
02863                                                                   
02864      MOVE EFFDTI                 TO  DC-GREG-DATE-1-EDIT.         
02865      MOVE '2'                    TO  DC-OPTION-CODE.              
02866      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
02867      IF DATE-CONVERSION-ERROR                                     
02868          MOVE ER-0215            TO  EMI-ERROR                    
02869          MOVE -1                 TO  EFFDTL                       
02870          MOVE AL-UNBON           TO  EFFDTA                       
02871          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
02872          GO TO 8200-SEND-DATAONLY                                 
02873      ELSE                                                         
02874          MOVE DC-BIN-DATE-1      TO  PI-CERT-EFF-DT               
02875                                      PI-CHEK-EFF-DT.              
02876                                                                   
02877  7090-EXIT.                                                       
02878       EXIT.                                                       
02879      EJECT                                                        
02880  7100-CREATE-TEMP-STORAGE.                                        
02881      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.             
02882                                                                   
02883      EXEC CICS WRITEQ TS                                          
02884          QUEUE   (QID)                                            
02885          FROM    (PROGRAM-INTERFACE-BLOCK)                        
02886          LENGTH  (PI-COMM-LENGTH)                                 
02887      END-EXEC.                                                    
02888                                                                   
02889  7100-EXIT.                                                       
02890       EXIT.                                                       
02891                                                                   
02892  7200-RECOVER-TEMP-STORAGE.                                       
02893      EXEC CICS READQ TS                                           
02894          QUEUE   (QID)                                            
02895          INTO    (PROGRAM-INTERFACE-BLOCK)                        
02896          LENGTH  (PI-COMM-LENGTH)                                 
02897      END-EXEC.                                                    
02898                                                                   
02899      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.             
02900                                                                   
02901  7200-EXIT.                                                       
02902       EXIT.                                                       
02903                                                                   
02904  7300-DELETE-TEMP-STORAGE.                                        
02905      EXEC CICS HANDLE CONDITION                                   
02906          QIDERR  (7300-EXIT)                                      
02907      END-EXEC.                                                    
02908                                                                   
02909      EXEC CICS DELETEQ TS                                         
02910          QUEUE  (QID)                                             
02911      END-EXEC.                                                    
02912                                                                   
02913  7300-EXIT.                                                       
02914       EXIT.                                                       
02915      EJECT                                                        

111418 7500-reissue-pass-1.
111418
111418     move '2'                    to pi-void-reissue-pass
111418
111418     move pi-void-reissue-amt    to amounto
111418
111418     if eibaid = dfhpf5
111418        MOVE PI-AM-NAME          TO PAYTO1I
111418        MOVE SPACES              TO PAYTO2I
111418        MOVE PI-AM-ADDRS         TO PAYAD1I
111418        MOVE PI-AM-CITY          TO PAYCTYI
111418        move pi-am-st            to paysti
111418        MOVE PI-AM-ZIP-CODE      TO PTOZIPI
111418        move 'ACCT'              to payeeo
111418        move al-panon            to payeea
111418
111418        MOVE AL-UANON            TO PAYTO1A
111418                                    PAYTO2A
111418                                    PAYAD1A
111418                                    PAYCTYA
111418                                    paysta
111418                                    PTOZIPA
111418                                    payeea
111418        GO TO 8100-SEND-INITIAL-MAP
111418     end-if
111418
111418     perform 5700-get-elcert     thru 5700-exit
111418     move spaces                 to pi-table-name
111418     string cm-insured-last-name ', '
111418            cm-insured-first-name ' '
111418            cm-insured-initial2
111418        delimited by '  '
111418        into
111418           pi-table-name
111418     end-string
111418
111418     IF CM-INSURED-FIRST-NAME GREATER SPACES                      
111418         MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME      
111418         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT      
111418     ELSE                                                         
111418         MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME      
111418         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.     
111418                                                                  
111418     MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.    
111418                                                                  
111418     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.                       
111418
111418     if eibaid = dfhpf7
111418        EXEC CICS READ                                               
111418            DATASET   (ERMAIL-FILE-ID)
111418            SET       (ADDRESS OF MAILING-DATA)
111418            RIDFLD    (ELCERT-KEY)
111418            resp      (ws-response)
111418        END-EXEC
111418        if resp-normal
111418           move spaces             to payto2i
111418           move ma-cred-bene-name  to payto1i
111418           MOVE MA-cred-bene-addr  TO PAYad1I
111418           MOVE MA-cred-bene-addr2 TO PAYad2I
111418           move ma-cred-bene-city  to payctyi
111418           move ma-cred-bene-state to paysti
111418           MOVE MA-cred-bene-zip   TO PTOZIPI
111418           move 'BENE'              to payeeo
111418           MOVE AL-UANON           TO payto1a
111418                                      PAYTO2A
111418                                      PAYad1A
111418                                      payad2a
111418                                      PAYctyA
111418                                      paysta
111418                                      PTOZIPA
111418                                      payeea
111418        end-if
111418     end-if
111418
111418     if eibaid = dfhpf6
111418        EXEC CICS READ
111418            DATASET   (ERMAIL-FILE-ID)
111418            SET       (ADDRESS OF MAILING-DATA)
111418            RIDFLD    (ELCERT-KEY)
111418            resp      (ws-response)
111418        END-EXEC
111418        if resp-normal
111418           move spaces            to payto2i
111418           move ws-name-work      to payto1i
111418           MOVE MA-ADDRESS-LINE-1 TO PAYAD1I
111418           MOVE MA-ADDRESS-LINE-2 TO PAYAD2I
111418           move ma-city           to payctyi
111418           move ma-addr-state     to paysti
111418           move 'INS'             to payeeo
111418           MOVE MA-ZIP            TO PTOZIPI
111418           MOVE AL-UANON          TO PAYAD1A
111418                                     PAYAD2A
111418                                     PAYctyA
111418                                     paysta
111418                                     PTOZIPA
111418                                     payto1a
111418                                     payeea
111418        end-if
111418     end-if
111418
111418     GO TO 8100-SEND-INITIAL-MAP
111418
111418     .
111418 7500-exit.
111418     exit.
111418
111418 7510-reissue-pass-2.
111418
111418     EXEC CICS READ
111418         DATASET  (ERCHEK-FILE-ID)
111418         SET      (ADDRESS OF CHECK-RECORDS)
111418         RIDFLD   (PI-ERCHEK-KEY)
111418         resp     (ws-response)
111418     END-EXEC
111418
111418     if not resp-normal
111418        go to 7510-exit
111418     end-if
111418
111418     if ch-void-dt not = save-bin-date
111418        display ' something went wrong here '
111418        go to 7510-exit
111418     end-if
111418
111418     MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS
111418     MOVE save-bin-date          TO CH-RECORDED-DT
111418                                    ch-released-dt
111418     move pi-processor-id        to ch-recorded-by
111418                                    ch-released-by
040919                                    ch-return-to
111418     move spaces                 to ch-check-no
111418                                    ch-void-by
111418                                    ch-void-reason
111418                                    ch-approved-by
111418                                    
111418
111418     move 'P'                    to ch-approval-status
111418
111418     MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT           
111418                                    CH-VOID-DT                    
111418                                    CH-CREDIT-ACCEPT-DT           
111418                                    ch-approval-dt
111418                                    ch-check-cashed-dt
111418     add +1 to ch-sequence-no
111418     if ch-check-origin-sw = 'R'
111418        move 'R'                 to pi-check-type
111418     else
111418        move 'C'                 to pi-check-type
111418     end-if
111418
111418     .
111418 7510-WRITE-RECORD.                                               
111418
111418     exec cics read
111418        dataset    (erchek-file-id)
111418        into       (ws-dummy-erchek)
111418        ridfld     (ch-control-primary)
111418        resp       (ws-response)
111418     end-exec

111418     if RESP-NOTFND
111418        continue
111418     else
111418        add +1                   to ch-sequence-no
111418        add 1                    to seqi
111418        go to 7510-write-record
111418     end-if
111418
111418     MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.                
111418     MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL          
111418                                    CH-CHECK-QUE-SEQUENCE
111418
111418     IF PAYTO1L NOT = ZEROS                                      
111418        MOVE PAYTO1I             TO CH-PAYEE-NAME-1
111418     END-IF
111418     IF PAYTO2L NOT = ZEROS                                       
111418        MOVE PAYTO2I             TO CH-PAYEE-NAME-2
111418     end-if
111418     IF PAYAD1L NOT = ZEROS                                       
111418        MOVE PAYAD1I             TO CH-PAYEE-ADDRESS-1
111418     end-if
111418     IF PAYAD2L NOT = ZEROS                                       
111418        MOVE PAYAD2I             TO CH-PAYEE-ADDRESS-2
111418     end-if
111418     IF PAYctyL NOT = ZEROS                                       
111418        MOVE payctyi             TO CH-PAYEE-CITY
111418     end-if
111418     IF PAYstL NOT = ZEROS                                       
111418        MOVE paysti              TO CH-PAYEE-state
111418     end-if
111418     IF PAYEEI GREATER SPACES                                     
111418        mOVE PAYEEI               TO CH-PAYEE-CODE
111418     end-if
111418     IF PTOZIPL  =  ZEROS                                         
111418        MOVE ZEROS               TO CH-PAYEE-ZIP-CODE
111418     else             
111418        MOVE PTOZIPI             TO WS-ZIP-CODE
111418        IF NOT WS-CANADIAN-POST-CODE                                 
111418           MOVE WS-ZIP-PRIME     TO CH-PAYEE-ZIP                  
111418           MOVE WS-ZIP-PLUS4     TO CH-PAYEE-ZIP-EXT
111418        end-if
111418     end-if
111418
111418     PERFORM 2700-WRITE-SQL      THRU 2700-EXIT
111418
111418     EXEC CICS WRITE                                              
111418         DATASET  (ERCHEK-FILE-ID)                                
111418         FROM     (CHECK-RECORDS)                                 
111418         RIDFLD   (CH-CONTROL-PRIMARY)                            
111418         resp     (ws-response)
111418     END-EXEC.                                                    

111418     COMPUTE WS-JOURNAL-RECORD-LENGTH =                           
111418             ERCHEK-RECORD-LENGTH + 23.                           
111418
111418     IF EMI-NO-ERRORS                                             
111418        MOVE ER-0000             TO EMI-ERROR                     
111418     ELSE                                                         
111418        IF EMI-FORCABLE-CTR GREATER THAN +0  AND                 
111418           EIBAID = DFHPF4                                       
111418           MOVE ER-2600          TO EMI-ERROR
111418        end-if
111418     end-if
111418
111418     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
111418                                                                  
111418     MOVE LOW-VALUES             TO EL677AI.                      
111418                                                                  
111418     MOVE PI-CHEK-CARRIER        TO CARRIERO.                     
111418     MOVE PI-CHEK-GROUPING       TO GROUPO.                       
111418     MOVE PI-CHEK-STATE          TO STATEO.                       
111418     MOVE PI-CHEK-ACCOUNT        TO ACCTO.                        
111418                                                                  
111418     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.                
111418     MOVE SPACE                  TO DC-OPTION-CODE.               
111418     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
111418     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.                       
111418                                                                  
111418     MOVE PI-CHEK-CERT-NO        TO CERTNOO.                      
111418     MOVE PI-CHEK-SUF-NO         TO SFXO.                         
111418     MOVE PI-CHEK-SEQUENCE       TO SEQO.                         
111418                                                                  
111418     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA      
111418                                    ACCTA     EFFDTA  CERTNOA     
111418                                    SFXA.                         
111418     MOVE AL-UNNON               TO SEQA
111418     move ' ' to pi-void-reissue-pass
111418     go to 5000-browse-file
111418
111418     .
111418 7510-exit.
111418     exit.

02926  8100-SEND-INITIAL-MAP.                                           
02927      MOVE SAVE-DATE              TO  DATEO.                       
02928      MOVE EIBTIME                TO  TIME-IN.                     
02929      MOVE TIME-OUT               TO  TIMEO.                       
02930      MOVE -1                     TO  MAINTL
           move al-uanon               to mainta

           move dedcyni                to pi-previous-deduct-comm
           if (PI-TO-EL677-FROM-EL1273)
              and (pi-void-reissue-pass = ' ')
111418        move al-sadof            to pf3a
111418                                    pf4a
111418                                    pf57a
111418                                    pf6a
                                          dprema
                                          dcomma
                                          drefa
                                          duecomma
                                          dprepda
                                          prema
                                          isscomma
                                          refa
                                          uecomma
                                          prepda
              move ', VOID(V)'         to dmaint1o
111418        move 'VOID & REISSUE(R), DELETE(D)'
111418                                 TO dmaint2o
           end-if

111418     if (pi-to-el677-from-el1273)
111418        and (pi-void-reissue-pass = '1')
111418        move al-sadof            to pf3a
111418                                    pf4a
111418                                    dprema
111418                                    dcomma
111418                                    drefa
111418                                    duecomma
111418                                    dprepda
111418                                    prema
111418                                    isscomma
111418                                    refa
111418                                    uecomma
111418                                    prepda
111418        move ', VOID(V)'         to dmaint1o
111418        move 'VOID AND REISSUE(R)'
111418                                 TO dmaint2o
111418     end-if

02931      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    
02932      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    
02933                                                                   
02943      EXEC CICS SEND                                               
02944          MAP      (EL677A)                                        
02945          MAPSET   (MAPSET-NAME)                                   
02946          FROM     (EL677AO)                                       
02947          ERASE                                                    
02948          CURSOR                                                   
02949      END-EXEC.                                                    
02950                                                                   
02951      GO TO 9100-RETURN-TRAN.                                      
02952      EJECT                                                        
02953  8200-SEND-DATAONLY.                                              

           if connected-to-db
              perform 4300-FINISH-UP-DB thru 4300-exit
           end-if

02954      MOVE SAVE-DATE              TO  DATEO.                       
02955      MOVE EIBTIME                TO  TIME-IN.                     
02956      MOVE TIME-OUT               TO  TIMEO.                       
02957      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    
02958      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    
02959                                                                   
02966      EXEC CICS SEND                                               
02967          MAP     (EL677A)                                         
02968          MAPSET  (MAPSET-NAME)                                    
02969          FROM    (EL677AO)                                        
02970          DATAONLY                                                 
02971          CURSOR                                                   
02972      END-EXEC.                                                    
02973                                                                   
02974      GO TO 9100-RETURN-TRAN.                                      
02975      EJECT                                                        
02976  8300-SEND-TEXT.                                                  
02977      EXEC CICS SEND TEXT                                          
02978          FROM     (LOGOFF-TEXT)                                   
02979          LENGTH   (LOGOFF-LENGTH)                                 
02980          ERASE                                                    
02981          FREEKB                                                   
02982      END-EXEC.                                                    
02983                                                                   
02984      EXEC CICS RETURN                                             
02985      END-EXEC.                                                    
02986                                                                   
02987  8500-DATE-CONVERT.                                               
02988      MOVE LINK-ELDATCV           TO  PGM-NAME.                    
02989                                                                   
02990      EXEC CICS LINK                                               
02991          PROGRAM    (PGM-NAME)                                    
02992          COMMAREA   (DATE-CONVERSION-DATA)                        
02993          LENGTH     (DC-COMM-LENGTH)                              
02994      END-EXEC.                                                    
02995                                                                   
02996  8500-EXIT.                                                       
02997       EXIT.                                                       
02998                                                                   
CIDMOD 8600-DEEDIT.                                                     
CIDMOD     EXEC CICS BIF DEEDIT                                         
CIDMOD         FIELD   (WS-DEEDIT-FIELD)                                
CIDMOD         LENGTH  (10)                                             
CIDMOD     END-EXEC.                                                    
CIDMOD                                                                  
CIDMOD 8600-EXIT.                                                       
CIDMOD      EXIT.                                                       
CIDMOD                                                                  
02999  8700-DEEDIT.                                                     
03000      EXEC CICS BIF DEEDIT                                         
03001          FIELD   (WS-DT-DEEDIT-FIELD)                             
03002          LENGTH  (10)                                             
03003      END-EXEC.                                                    
03004                                                                   
03005  8700-EXIT.                                                       
03006       EXIT.                                                       
03007                                                                   
03008  8800-UNAUTHORIZED-ACCESS.                                        
03009      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  
03010      GO TO 8300-SEND-TEXT.                                        
03011                                                                   
03012  8810-PF23.                                                       
03013      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
03014      MOVE XCTL-005               TO  PGM-NAME.                    
03015      GO TO 9300-XCTL.                                             
03016                                                                   
03017                                                                   
03018  9100-RETURN-TRAN.                                                
03019      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
03020      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        
03021      EXEC CICS RETURN                                             
03022          TRANSID  (TRANS-ID)                                      
03023          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
03024          LENGTH   (PI-COMM-LENGTH)                                
03025      END-EXEC.                                                    
03026                                                                   
03027      GOBACK.                                                      
03028                                                                   
03029  9200-RETURN-MAIN-MENU.                                           
03030      MOVE XCTL-626               TO  PGM-NAME.                    
03031      GO TO 9300-XCTL.                                             
03032                                                                   
03033  9300-XCTL.                                                       
03034      EXEC CICS XCTL                                               
03035          PROGRAM    (PGM-NAME)                                    
03036          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
03037          LENGTH     (PI-COMM-LENGTH)                              
03038      END-EXEC.                                                    
03039                                                                   
03040  9400-CLEAR.                                                      
03041      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    
03042      MOVE SPACES                 TO  PI-PFKEY.                    
03043      GO TO 9300-XCTL.                                             
03044                                                                   
03045  9500-PF12.                                                       
03046      MOVE XCTL-010               TO  PGM-NAME.                    
03047      GO TO 9300-XCTL.                                             
03048                                                                   
03049  9600-PGMID-ERROR.                                                
03050      EXEC CICS HANDLE CONDITION                                   
03051          PGMIDERR    (8300-SEND-TEXT)                             
03052      END-EXEC.                                                    
03053                                                                   
03054      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          
03055      MOVE ' '                    TO  PI-ENTRY-CD-1.               
03056      MOVE XCTL-005               TO  PGM-NAME.                    
03057      MOVE PGM-NAME               TO  LOGOFF-PGM.                  
03058      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
03059                                                                   
03060      GO TO 9300-XCTL.                                             
03061                                                                   
03062  9900-ERROR-FORMAT.                                               
03063      IF NOT EMI-ERRORS-COMPLETE                                   
03064          MOVE LINK-001           TO  PGM-NAME                     
03065          EXEC CICS LINK                                           
03066              PROGRAM    (PGM-NAME)                                
03067              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
03068              LENGTH     (EMI-COMM-LENGTH)                         
03069          END-EXEC.                                                
03070                                                                   
03071  9900-EXIT.                                                       
03072       EXIT.                                                       
03073                                                                   
03074  9990-ABEND.                                                      
03075      MOVE LINK-004               TO  PGM-NAME.                    
03076      MOVE DFHEIBLK               TO  EMI-LINE1.                   
03077      EXEC CICS LINK                                               
03078          PROGRAM   (PGM-NAME)                                     
03079          COMMAREA  (EMI-LINE1)                                    
03080          LENGTH    (72)                                           
03081      END-EXEC.                                                    
03082                                                                   
03083      GO TO 8200-SEND-DATAONLY.                                    
03084                                                                   
03085  9995-SECURITY-VIOLATION.                                         
03086                              COPY ELCSCTP.                        
03087                                                                   
03088  9995-EXIT.                                                       
03089      EXIT.                                                        
03090                                                                   
