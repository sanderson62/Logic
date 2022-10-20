      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL691.
00004 *
00005 *AUTHOR.    LOGIC, INC.
00006 *           DALLAS, TEXAS.
00007
00008 *DATE-COMPILED.
00009
00010 *REMARKS.
00011 *        THIS PROGRAM PROVIDES THE FUNCTIONS TO BROWSE AND EDIT
00012 *    THE DETAIL OF AN ARCHIVED LETTER.
00013
00014 *    TRANS ID = EXN1
00015
071111******************************************************************
071111*                   C H A N G E   L O G
071111*
071111* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071111*-----------------------------------------------------------------
071111*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071111* EFFECTIVE    NUMBER
071111*-----------------------------------------------------------------
071111* 071111    2011022800001  AJRA  NAPERSOFT - NEW SCREEN
100312* 100312    2011022800001  AJRA  CHANGE RECEIVED MSG
121112* 121112    2012101700002  AJRA  ADD PF6 TO CERT NOTE, ENDT ARCH NO
122612* 122612    2012101700002  AJRA  UPDATE BILLING NOTE ON RECEIVED
100813* 100813    2013100700002  AJRA  VERITY RESEND LETTER ID ON CHANGE
102918* 102918  CR2018080300002  PEMA  ADD VOID OB LETTER OPTION
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
102020* 102020 IR2020101300001   PEMA  Correct billing notes
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
071111******************************************************************
00016
00017  DATA DIVISION.
00018  WORKING-STORAGE SECTION.
00019  77  FILLER  PIC X(32) VALUE '********************************'.
00020  77  FILLER  PIC X(32) VALUE '*    EL691 WORKING STORAGE     *'.
00021  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
102918 77  c1                          pic s9(5) comp-3 value +0.
102918 77  n1                          pic s999  comp-3 value +0.
102918 77  w-comment-line-cnt          pic s999 comp-3 value +0.
102918 77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
102918 77  note-count                  pic s999 comp-3 value +0.
102918 77  ws-build-note-sw            pic x value ' '.
102918     88  finished-with-notes      value 'Y'.
102918 77  ws-ercnot-sw                pic x  value spaces.
102918     88  ercnot-startbr            value 'Y'.
102918
102918 01  P pointer.
102918 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
102918 01  var-ptr pointer.
102918 01  env-var-len                 pic 9(4)  binary.
102918 01  rc                          pic 9(9)  binary.
102918
102918 01  WS-KIXSYS.
102918     05  WS-KIX-FIL1             PIC X(10).
102918     05  WS-KIX-APPS             PIC X(10).
102918     05  WS-KIX-ENV              PIC X(10).
102918     05  WS-KIX-MYENV            PIC X(10).
102918     05  WS-KIX-SYS              PIC X(10).

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
       01  ws-sql-code                 pic s9(7) value zeros.
       01  ws-dis-sql-code             pic -9999999 value zeros.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch keyword into                                  ***
      ***           :db-Complete-dt :nu-Complete-date,               ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-Complete-date        pic s9(4) comp value +0.

       01  key-word-key-data.
           05  kd-unique-id            pic 9(7).
           05  kd-cert-no              pic x(11).

       01  key-word-table-data.
           05  db-req-date             pic x(10).
           05  db-doc-type             pic x(30).
           05  db-Cert-no              pic x(11).
           05  db-Print-dt             pic x(10).
           05  db-Cert-exp-dt          pic x(10).
           05  db-cert-state           pic xx.
           05  db-key-word-type        pic x(30).
           05  db-key-word-value       pic x(30).
           05  db-complete-dt          pic x(10).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

00023
00024  01  W-WORK-AREAS.
102918     12  ws-sql-update-sw        pic x value ' '.
102918         88  sql-update-succeeded    value 'Y'.
102918         88  sql-update-failed       value 'N'.
102918     12  ws-connect-sw           pic x  value ' '.
102918         88  connected-to-db        value 'Y'.
102918     12  ws-cert-sw              pic x value ' '.
102918         88  cert-found             value 'Y'.
00025      12  FILLER                  PIC  X(18)
00026                                       VALUE 'PROGRAM WORK AREA:'.
00027      12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
00028      12  W-CALL-PGM              PIC  X(08).
00029      12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
00030      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
00031      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
00032
00033      12  W-TIME-IN               PIC S9(07).
00034      12  FILLER REDEFINES W-TIME-IN.
00035          16  FILLER              PIC  X(01).
00036          16  W-TIME-OUT          PIC  9(02)V9(02).
00037          16  FILLER              PIC  X(02).
00038
00039      12  W-DEEDIT-FIELD          PIC  X(15).
00040      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
00041
00042      12  W-ERROR-COUNT           PIC S9(3)       VALUE ZERO.
00043      12  W-UPDATE-SW             PIC S9          VALUE ZERO.
00044      12  W-RESEND-DATE           PIC XX    VALUE LOW-VALUES.
00045      12  W-SENT-DATE             PIC XX    VALUE LOW-VALUES.
00046      12  W-FINAL-ACT-DATE        PIC XX    VALUE LOW-VALUES.
00047      12  W-RECEIVED-DATE         PIC XX    VALUE LOW-VALUES.
00048      12  W-STOP-LETTER-DATE      PIC XX    VALUE LOW-VALUES.
00049      12  W-RESPONSE              PIC S9(8)   COMP.
00050          88  RESP-NORMAL                  VALUE +00.
00051          88  RESP-NOTFND                  VALUE +13.
00052          88  RESP-DUPREC                  VALUE +14.
00053          88  RESP-DUPKEY                  VALUE +15.
00054          88  RESP-NOTOPEN                 VALUE +19.
00055          88  RESP-ENDFILE                 VALUE +20.
102918         88  resp-lengtherr               value +22.
00056
00057      12  W-ARCH-SAVE-KEY         PIC  X(03).
00058      12  W-ARCH-KEY.
00059          16  W-ARCH-COMPANY-CD   PIC  X(01).
00060          16  W-ARCH-NUMBER       PIC S9(08)      COMP.
00061
00062      12  W-ARCT-KEY.
00063          16  W-ARCT-COMPANY-CD   PIC  X(01).
00064          16  W-ARCT-ARCHIVE-NO   PIC S9(08) COMP.
00065          16  W-ARCT-RECORD-TYPE  PIC  X(01).
00066              88  W-ARCT-COMMENT-DATA   VALUE '3'.
00067          16  W-ARCT-LINE-SEQ-NO  PIC S9(04) COMP.
00068
102918     12  w-stop-letter-comment   pic x(126) value spaces.
102918     12  w-cert-note-comment     pic x(70) value spaces.
00069      12  W-CERT-NOTE-MSG.
00070          16  FILLER              PIC X(29)
100312             VALUE 'REQUESTED DOCUMENT RECEIVED  '.
00072          16  W-CERT-NOTE-RECV-DT PIC X(8).
122612
122612     12  WS-FIND-BILLING-NOTE.
122612         16  WS-FBN-NOTE         PIC X(25).
122612         16  WS-FBN-LTRID        PIC X(4).
122612
122612     12  WS-RECEIVED-NOTE.
122612         16  FILLER              PIC X(12) 
122612             VALUE ' - RECEIVED '.
122612         16  WS-BN-RECV-DATE     PIC X(8).
100813
100813     12  WS-Z-RECORD-IND         PIC X(1) VALUE 'N'.
100813         88 Z-RECORD-FOUND                VALUE 'Y'.
100813         88 Z-RECORD-NOT-FOUND            VALUE 'N'.
00073          
00074      12  ELCERT-FILE-ID          PIC  X(08)  VALUE 'ELCERT'.
00075      12  ERCNOT-FILE-ID          PIC  X(08)  VALUE 'ERCNOT'.
00076      12  ERCNOT-LENGTH           PIC S9(4)   COMP VALUE +150. 
00077      12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.  
00078      12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.  
00079      12  ERCNOT-KEY.                                          
00080          16  ERCNOT-PARTIAL-KEY.                              
00081              20 ERCNOT-COMPANY-CD    PIC X.                   
00082              20 ERCNOT-CARRIER       PIC X.                   
00083              20 ERCNOT-GROUPING      PIC X(06).               
00084              20 ERCNOT-STATE         PIC XX.                  
00085              20 ERCNOT-ACCOUNT       PIC X(10).               
00086              20 ERCNOT-EFF-DT        PIC XX.                  
00087              20 ERCNOT-CERT-NO.                               
00088                 25 ERCNOT-CERT-PRIME PIC X(10).               
00089                 25 ERCNOT-CERT-SFX   PIC X.                   
00090              20 ERCNOT-REC-TYP       PIC X.                   
00091          16 ERCNOT-SEQ           PIC S9(4) COMP.              
00092      12  SV-PRIOR-KEY.                                        
00093          16 SV-COMPANY-CD            PIC X.                   
00094          16 SV-CARRIER               PIC X.                   
00095          16 SV-GROUPING              PIC X(06).               
00096          16 SV-STATE                 PIC XX.                  
00097          16 SV-ACCOUNT               PIC X(10).               
00098          16 SV-EFF-DT                PIC XX.                  
00099          16 SV-CERT-NO.                                       
00100             20 SV-CERT-PRIME         PIC X(10).               
00101             20 SV-CERT-SFX           PIC X(1).                
00102          16 SV-REC-TYP               PIC X.                   
00103          16  SV-NOTE-SEQUENCE        PIC S9(4) COMP.
00104      12  ELCERT-KEY.                                          
00105          16  ELCERT-COMPANY-CD        PIC X.                  
00106          16  ELCERT-CARRIER           PIC X.                  
00107          16  ELCERT-GROUPING          PIC X(6).               
00108          16  ELCERT-STATE             PIC XX.                 
00109          16  ELCERT-ACCOUNT           PIC X(10).              
00110          16  ELCERT-EFF-DT            PIC XX.                 
00111          16  ELCERT-CERT-NO.                                  
00112              20  ELCERT-CERT-PRIME    PIC X(10).              
00113              20  ELCERT-CERT-SFX      PIC X.                  
122612     12  ELEOBC-FILE-ID          PIC X(08)  VALUE 'ELEOBC'.
122612     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
122612     12  ELEOBC-KEY.
122612         16  EOBC-COMPANY-CD     PIC X.
122612         16  EOBC-REC-TYPE       PIC X.
122612         16  EOBC-CODE           PIC X(4).
122612         16  FILLER              PIC X(9).
122612     12  ERNOTE-FILE-ID          PIC X(08)  VALUE 'ERNOTE'.
122612     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
122612     12  ERNOTE-KEY.
122612         16  ERNOTE-COMPANY-CD   PIC X.
122612         16  ERNOTE-CARRIER      PIC X.
122612         16  ERNOTE-GROUPING     PIC X(6).
122612         16  ERNOTE-STATE        PIC XX.
122612         16  ERNOTE-ACCOUNT      PIC X(10).
122612         16  ERNOTE-CERT-EFF-DT  PIC XX.
122612         16  ERNOTE-CERT-PRIME   PIC X(10).
122612         16  ERNOTE-CERT-SFX     PIC X.
041320         16  ernote-record-type  pic x.
122612     12  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
100813     12  ELLETR-KEY.
100813         16  LETR-PART-KEY.
100813             20  LETR-COMPANY-CD PIC X.
100813             20  LETR-LETTER-ID  PIC X(4).
100813         16  LETR-FILLER         PIC X(8).
100813         16  LETR-SEQ-NO         PIC 9(4) BINARY.
100813     12  ELLETR-SAVE-PART-KEY    PIC X(5).

102918 01  w-comment-line-1            pic x(63) value spaces.
102918 01  w-comment-line-2            pic x(63) value spaces.
102918 01  cert-note-records-holder.
102918     05  cert-note-record occurs 500.
102918         10  filler              pic x(48).
102918         10  cnr-rest            pic x(102).

00114          
00115
00116  01  FILLER                      PIC  X(22)
00117                                  VALUE 'INTERFACE AREA STARTS:'.
00118      COPY ELCINTF.
00119      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00120      COPY ELC1042.
00121      COPY ELC689PI.
00122          16  PI-690-WORK-AREA.                                    
00123              20  PI-690-ARCHIVE-TABLE.                            
00124                  24  PI-690-ARCHIVE-NUM OCCURS 12 TIMES           
00125                                  PIC S9(08) COMP.                 
00126              20  PI-690-CURSOR   PIC S9(04) COMP.                 
00127              20  PI-690-FIRST-DATA.                               
00128                  24  PI-690-FIRST-CERT-NO.                        
00129                      28  PI-690-FIRST-CERT-PRIME                  
00130                                  PIC  X(10).                      
00131                      28  PI-690-FIRST-SUFFIX                      
00132                                  PIC  X(01).                      
00133                  24  PI-690-FIRST-CARRIER                         
00134                                  PIC  X(01).                      
00135                  24  PI-690-FIRST-GROUPING                        
00136                                  PIC  X(06).                      
00137                  24  PI-690-FIRST-STATE                           
00138                                  PIC  X(02).                      
00139                  24  PI-690-FIRST-ACCOUNT                         
00140                                  PIC  X(10).                      
00141                  24  PI-690-FIRST-EFFECT-DATE                     
00142                                  PIC  X(02).                      
00143                  24  PI-690-FIRST-ENTRY.                          
00144                      28  PI-690-FIRST-CONTROL-PREFIX              
00145                                  PIC  X(02).                      
00146                      28  PI-690-FIRST-CONTROL                     
00147                                  PIC S9(08) COMP.                 
00148                  24  PI-690-FIRST-FORM                            
00149                                  PIC  X(04).                      
00150                  24  PI-690-FIRST-PROCESSOR                       
00151                                  PIC  X(04).                      
00152                  24  PI-690-FIRST-ARCHIVE-NO                      
00153                                  PIC S9(08) COMP.                 
00154              20  PI-690-INIT-DATA.                                
00155                  24  PI-690-INIT-CERT-NO.                         
00156                      28  PI-690-INIT-CERT-PRIME                   
00157                                  PIC  X(10).                      
00158                      28  PI-690-INIT-SUFFIX                       
00159                                  PIC  X(01).                      
00160                  24  PI-690-INIT-CARRIER                          
00161                                  PIC  X(01).                      
00162                  24  PI-690-INIT-GROUPING                         
00163                                  PIC  X(06).                      
00164                  24  PI-690-INIT-STATE                            
00165                                  PIC  X(02).                      
00166                  24  PI-690-INIT-ACCOUNT                          
00167                                  PIC  X(10).                      
00168                  24  PI-690-INIT-EFFECT-DATE                      
00169                                  PIC  X(02).                      
00170                  24  PI-690-INIT-EFF-DTE                          
00171                                  PIC  X(08).                      
00172                  24  PI-690-INIT-ENTRY.                           
00173                      28  PI-690-INIT-CONTROL-PREFIX               
00174                                  PIC  X(02).                      
00175                      28  PI-690-INIT-CONTROL                      
00176                                  PIC S9(08) COMP.                 
00177                  24  PI-690-INIT-FORM                             
00178                                  PIC  X(04).                      
00179                  24  PI-690-INIT-PROCESSOR                        
00180                                  PIC  X(04).                      
00181                  24  PI-690-INIT-ARCHIVE-NO                       
00182                                  PIC S9(08) COMP.                 
00183              20  PI-690-LAST-DATA.                                
00184                  24  PI-690-LAST-CERT-NO.                         
00185                      28  PI-690-LAST-CERT-PRIME                   
00186                                  PIC  X(10).                      
00187                      28  PI-690-LAST-SUFFIX                       
00188                                  PIC  X(01).                      
00189                  24  PI-690-LAST-CARRIER                          
00190                                  PIC  X(01).                      
00191                  24  PI-690-LAST-GROUPING                         
00192                                  PIC  X(06).                      
00193                  24  PI-690-LAST-STATE                            
00194                                  PIC  X(02).                      
00195                  24  PI-690-LAST-ACCOUNT                          
00196                                  PIC  X(10).                      
00197                  24  PI-690-LAST-EFFECT-DATE                      
00198                                  PIC  X(02).                      
00199                  24  PI-690-LAST-ENTRY.                           
00200                      28  PI-690-LAST-CONTROL-PREFIX               
00201                                  PIC  X(02).                      
00202                      28  PI-690-LAST-CONTROL                      
00203                                  PIC S9(08) COMP.                 
00204                  24  PI-690-LAST-FORM                             
00205                                  PIC  X(04).                      
00206                  24  PI-690-LAST-PROCESSOR                        
00207                                  PIC  X(04).                      
00208                  24  PI-690-LAST-ARCHIVE-NO                       
00209                                  PIC S9(08) COMP.                 
00210              20  PI-690-LAST-ARCH-NDX                             
00211                                  PIC S9(04) COMP.                 
00212              20  PI-690-BRWS-TYPE-IND                             
00213                                  PIC  9(01).                      
00214                  88  PI-690-BRWS-CERTRP               VALUE 1.    
00215                  88  PI-690-BRWS-FORM                 VALUE 2.    
00216                  88  PI-690-BRWS-PROCESSOR            VALUE 3.    
00217                  88  PI-690-BRWS-ACCOUNT              VALUE 4.    
00218                  88  PI-690-BRWS-ENTRY-CNTL           VALUE 5.    
00219                  88  PI-690-BRWS-ARCHIVE              VALUE 6.    
00220              20  PI-690-LAST-BROWSE-IND                           
00221                                  PIC  X(01).                      
00222                  88  PI-690-LAST-BRWS-FWRD            VALUE '1'.  
00223                  88  PI-690-LAST-BRWS-BWRD            VALUE '2'.  
00224              20  PI-690-STATUS-SELECTION-IND                      
00225                                  PIC  X(01).                      
00226                  88  PI-690-SELECT-ALL       VALUE 'N'.           
00227                  88  PI-690-VALID-SELECTION  VALUE 'A' 'C' 'H'    
00228                                                'X' 'P' 'V' 'N'.   
00229          16  PI-ARCHIVE-COMPLETE    PIC X(01).
00230          16  PI-ARCHIVE-RECEIVED    PIC X(01).
00231          16  PI-ARCHIVE-STOPPED     PIC X(01).
00232          16  PI-ARCHIVE-FINAL       PIC X(01).
00233          16  PI-COMMENT-INDEX       PIC S9(4) COMP.
122612         16  PI-ARCHIVE-LTRID       PIC X(4).
102918         16  pi-create-date         pic x(08).
102918         16  pi-initial-print-date  pic xx.
102918         16  FILLER              PIC X(49).                       
00236
00237  01  W-CONSTANT-AREA.
00238      12  FILLER                  PIC  X(18)
00239                                  VALUE 'PROGRAM CONSTANTS:'.
00240      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
00241
00242      12  W-ARCH-FILE-ID          PIC  X(08)  VALUE 'ERARCH'.
00243      12  W-ARCT-FILE-ID          PIC  X(08)  VALUE 'ERARCT'.
00244      12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
00245      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.
00246      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.
00247      12  W-MAP                   PIC  X(08)  VALUE 'EL691A'.
00248      12  W-MAP-REDEFINE  REDEFINES   W-MAP.
00249          16  FILLER              PIC  X(02).
00250          16  W-MAP-NUM           PIC  X(06).
00251      12  W-MAPSET                PIC  X(08)  VALUE 'EL691S'.
00252      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL691'.
00253      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXN1'.  
00254      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.
00255      12  W-XCTL-626              PIC  X(08)  VALUE 'EL626'.
121112     12  W-XCTL-1279             PIC  X(08)  VALUE 'EL1279'.
00256      12  SLASH                   PIC X       VALUE '/'.
00257      12  W-ZEROS                 PIC  S9(03) VALUE +0 COMP-3.
00258      12  W-ADD-ARCT              PIC  X      VALUE 'N'.
00259      12  W-DONE-ADDING           PIC  X      VALUE 'N'.
00260      12  W-NEED-COMMENT          PIC  X      VALUE 'N'.
00261
00262  01  ERROR-MESSAGES.
00263      12  ER-0000                 PIC  X(04) VALUE '0000'.
00264      12  ER-0004                 PIC  X(04) VALUE '0004'.
00265      12  ER-0029                 PIC  X(04) VALUE '0029'.
00266      12  ER-0051                 PIC  X(04) VALUE '0051'.
00267      12  ER-0070                 PIC  X(04) VALUE '0070'.
102918     12  ER-0249                 PIC  X(04) VALUE '0249'.
00268      12  ER-0295                 PIC  X(04) VALUE '0295'.
00269      12  ER-0296                 PIC  X(04) VALUE '0296'.
00270      12  ER-0539                 PIC  X(04) VALUE '0539'.
00271      12  ER-0895                 PIC  X(04) VALUE '0895'.
00272      12  ER-0897                 PIC  X(04) VALUE '0897'.
100813     12  ER-1236                 PIC  X(04) VALUE '1236'.
00273      12  ER-3112                 PIC  X(04) VALUE '3112'.
00274      12  ER-7008                 PIC  X(04) VALUE '7008'.
102918     12  ER-7330                 PIC  X(04) VALUE '7330'.
102918     12  ER-7331                 PIC  X(04) VALUE '7331'.
102918     12  ER-7332                 PIC  X(04) VALUE '7332'.
102918     12  ER-7333                 PIC  X(04) VALUE '7333'.
102918     12  ER-7334                 PIC  X(04) VALUE '7334'.
00275      12  ER-7363                 PIC  X(04) VALUE '7363'.
102918     12  ER-7364                 PIC  X(04) VALUE '7364'.
00276      12  ER-7371                 PIC  X(04) VALUE '7371'.
00277      12  ER-7373                 PIC  X(04) VALUE '7373'.
00278      12  ER-7388                 PIC  X(04) VALUE '7388'.
00279      12  ER-9097                 PIC  X(04) VALUE '9097'.
00280      12  ER-9245                 PIC  X(04) VALUE '9245'.

00282                                  COPY ELCAID.
00283  01  FILLER    REDEFINES DFHAID.
00284      12  FILLER                  PIC  X(08).
00285      12  PF-VALUES               PIC  X(01) OCCURS 2.

00287                                  COPY ELCATTR.
00289                                  COPY ELCDATE.
100813                                 COPY ELCTEXT.
00291                                  COPY ELCNWA.
00293                                  COPY ELCEMIB.
                                       COPY ERCCNOT.

00294  01  EMI-SAVE-AREA               PIC X(400).

00296                                  COPY ELCLOGOF.
00298                                  COPY ELCSCTM.
00300                                  COPY ELCSCRTY.
00303                                  COPY EL691S.

00305  LINKAGE SECTION.
00306  01  DFHCOMMAREA                 PIC X(1024).
00308                                  COPY ERCARCH.
00310                                  COPY ERCARCT.
00314                                  COPY ELCCERT.
122612                                 COPY ELCEOBC.
122612                                 COPY ERCNOTE.
102918 01  var                         pic x(30).

00317  PROCEDURE DIVISION.
00318      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00319
00320      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00321      MOVE '5'                    TO DC-OPTION-CODE.
00322      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
00323      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00324      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE
00325                                     W-CURRENT-SAVE.
00326
00327      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00328      MOVE ERROR-MESSAGE-INTERFACE-BLOCK
00329                                  TO EMI-SAVE-AREA.
00330
00331      IF  EIBCALEN EQUAL 0
00332          MOVE UNACCESS-MSG       TO LOGOFF-MSG
00333          GO TO 8300-SEND-TEXT
00334      END-IF.

102918     set P to address of KIXSYS
102918     CALL "getenv" using by value P returning var-ptr
102918     if var-ptr = null then
102918        display ' kixsys not set '
102918     else
102918        set address of var to var-ptr
102918        move 0 to env-var-len
102918        inspect var tallying env-var-len
102918          for characters before X'00' 
102918        unstring var (1:env-var-len) delimited by '/'
102918           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
102918              WS-KIX-SYS
102918        end-unstring
102918     end-if

00336      IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
00337          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
00338              MOVE PI-SAVED-PROGRAM-5
00339                                  TO PI-SAVED-PROGRAM-6
00340              MOVE PI-SAVED-PROGRAM-4
00341                                  TO PI-SAVED-PROGRAM-5
00342              MOVE PI-SAVED-PROGRAM-3
00343                                  TO PI-SAVED-PROGRAM-4
00344              MOVE PI-SAVED-PROGRAM-2
00345                                  TO PI-SAVED-PROGRAM-3
00346              MOVE PI-SAVED-PROGRAM-1
00347                                  TO PI-SAVED-PROGRAM-2
00348              MOVE PI-RETURN-TO-PROGRAM
00349                                  TO PI-SAVED-PROGRAM-1
00350              MOVE PI-CALLING-PROGRAM
00351                                  TO PI-RETURN-TO-PROGRAM
00352              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
00353          ELSE
00354              MOVE PI-CALLING-PROGRAM TO W-CALL-PGM
00355              MOVE PI-RETURN-TO-PROGRAM
00356                                      TO PI-CALLING-PROGRAM
00357              MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM
00358              MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1
00359              MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2
00360              MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3
00361              MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4
00362              MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5
00363              MOVE SPACES             TO PI-SAVED-PROGRAM-6
00364          END-IF
00365      ELSE
00366          GO TO 0200-RECEIVE
00367      END-IF.
00368
00369      GO TO 1000-SHOW.
00370
00371                                  EJECT
00372
00373  0200-RECEIVE.
00374
00375      EXEC CICS HANDLE AID
00376          CLEAR    (9300-DFHCLEAR)
00377          PA1      (9200-PA)
00378          PA2      (9200-PA)
00379          PA3      (9200-PA)
00380      END-EXEC.
00381
00382      EXEC CICS HANDLE CONDITION
00383          PGMIDERR (9700-PGMID-ERROR)
00384          ERROR    (9800-ABEND)
00385      END-EXEC.
00386
00387      EXEC CICS RECEIVE
00388          MAP      (W-MAP)
00389          MAPSET   (W-MAPSET)
00390          INTO     (EL691AI)
00391      END-EXEC.
00392
00393      IF  NOT DISPLAY-CAP
00394          MOVE 'READ'             TO SM-READ
00395          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00396          MOVE ER-9097            TO EMI-ERROR
00397          MOVE -1                 TO MAINTL
00398          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00399          GO TO 8100-SEND-INITIAL-MAP
00400      END-IF.
00401
00402
00403  0300-CHECK-PFKEYS.
00404
00405      IF  EIBAID EQUAL DFHPF23
00406          MOVE EIBAID             TO PI-ENTRY-CD-1
00407          MOVE W-XCTL-005         TO W-CALL-PGM
00408          GO TO 9400-XCTL
00409      END-IF.
00410
00411      IF  EIBAID EQUAL DFHPF24
00412          MOVE W-XCTL-626         TO W-CALL-PGM
00413          GO TO 9400-XCTL
00414      END-IF.
121112
121112     IF  EIBAID EQUAL DFHPF6
102020         move pi-689-chg-seq-nox(1:1)
102020                                 to PI-PROGRAM-WORK-AREA(1:1)
121112         MOVE W-XCTL-1279        TO W-CALL-PGM
121112         GO TO 9400-XCTL
121112     END-IF
00415
00416      IF  MAINTI EQUAL 'C'  AND
00417          EIBAID EQUAL DFHENTER
00418           GO TO 0700-PROCESS-CHANGES
00419      END-IF.
00420
00421      MOVE ER-0029                TO EMI-ERROR.
00422
00423
00424  0320-INPUT-ERROR.
00425
00426      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00427      MOVE -1                     TO  MAINTL.
00428      GO TO 8200-SEND-DATAONLY.
00429
00430
00431  0700-PROCESS-CHANGES.
00432
00433      IF  NOT MODIFY-CAP
00434          MOVE 'UPDATE'           TO SM-READ
00435          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00436          MOVE ER-0070            TO EMI-ERROR
00437          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00438          MOVE -1                 TO MAINTL
00439          GO TO 8100-SEND-INITIAL-MAP
00440      END-IF.
00441
100813     IF RESFORML > ZEROS
100813        AND RESFORMI > SPACES
100813        MOVE PI-COMPANY-CD       TO LETR-COMPANY-CD
100813        MOVE RESFORMI            TO LETR-LETTER-ID
100813        MOVE LETR-PART-KEY       TO ELLETR-SAVE-PART-KEY
100813        MOVE SPACES              TO LETR-FILLER
100813        MOVE 0                   TO LETR-SEQ-NO
100813        PERFORM 1500-CHECK-Z-RECORD THRU 1500-EXIT
100813        IF Z-RECORD-NOT-FOUND
100813           MOVE ER-1236          TO EMI-ERROR
100813           MOVE -1               TO RESFORML
100813           MOVE AL-UABON         TO RESFORMA
100813           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100813        END-IF
100813     END-IF
100813
00442      MOVE 'N'                    TO W-NEED-COMMENT.
00443
00444      IF RESENDL GREATER ZERO
00445          MOVE 'Y'                TO W-NEED-COMMENT
00446          IF RESENDI = SPACES
00447              MOVE AL-UANON       TO  RESENDA
00448              MOVE +1             TO  W-UPDATE-SW
00449              MOVE LOW-VALUES     TO  W-RESEND-DATE
00450          ELSE
00451              MOVE RESENDI       TO  W-DEEDIT-FIELD
00452              PERFORM 8600-DEEDIT THRU 8600-EXIT
00453              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00454                  MOVE W-DEEDIT-FIELD-V0  TO  RESENDO
00455                  INSPECT RESENDI CONVERTING SPACES TO SLASH
00456                  MOVE '4'        TO  DC-OPTION-CODE
00457                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00458                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00459                  IF DC-ERROR-CODE NOT = SPACES
00460                      MOVE ER-0295         TO  EMI-ERROR
00461                      MOVE -1              TO  RESENDL
00462                      MOVE AL-UABON        TO  RESENDA
00463                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00464                  ELSE
00465                      MOVE AL-UANON       TO  RESENDA
00466                      MOVE +1             TO  W-UPDATE-SW
00467                      MOVE DC-BIN-DATE-1  TO  W-RESEND-DATE
00468                  END-IF
00469              ELSE
00470                  MOVE ER-0295    TO  EMI-ERROR
00471                  MOVE -1         TO  RESENDL
00472                  MOVE AL-UABON   TO  RESENDA
00473                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00474              END-IF
00475          END-IF
00476      END-IF.
00477
00478      IF FINDATEL GREATER ZERO
00479          IF FINDATEI = SPACES
00480              MOVE AL-UANON       TO  FINDATEA
00481              MOVE +1             TO  W-UPDATE-SW
00482              MOVE LOW-VALUES     TO  W-FINAL-ACT-DATE
00483              MOVE SPACES         TO  PI-ARCHIVE-FINAL
00484          ELSE
00485              MOVE 'Y'             TO  PI-ARCHIVE-FINAL
00486              MOVE FINDATEI        TO  W-DEEDIT-FIELD
00487              PERFORM 8600-DEEDIT THRU 8600-EXIT
00488              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00489                  MOVE W-DEEDIT-FIELD-V0  TO  FINDATEO
00490                  INSPECT FINDATEI CONVERTING SPACES TO SLASH
00491                  MOVE '4'                 TO  DC-OPTION-CODE
00492                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00493                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00494                  IF DC-ERROR-CODE NOT = SPACES
00495                      MOVE ER-0296        TO  EMI-ERROR
00496                      MOVE -1             TO  FINDATEL
00497                      MOVE AL-UABON       TO  FINDATEA
00498                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00499                  ELSE
00500                      MOVE AL-UANON       TO  FINDATEA
00501                      MOVE +1             TO  W-UPDATE-SW
00502                      MOVE DC-BIN-DATE-1  TO  W-FINAL-ACT-DATE
00503                  END-IF
00504              ELSE
00505                  MOVE ER-0296    TO  EMI-ERROR
00506                  MOVE -1         TO  FINDATEL
00507                  MOVE AL-UABON   TO  FINDATEA
00508                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00509              END-IF
00510          END-IF
00511      END-IF.
00512
00513      IF REPLYL GREATER ZERO
00514          IF REPLYI = SPACES
00515              MOVE AL-UANON       TO  REPLYA
00516              MOVE +1             TO  W-UPDATE-SW
00517              MOVE LOW-VALUES     TO  W-RECEIVED-DATE
00518              MOVE SPACES         TO  PI-ARCHIVE-RECEIVED
122612                                     WS-RECEIVED-NOTE
122612             PERFORM 4500-UPDATE-BILL-NOTE THRU 4500-EXIT
00519          ELSE
00520              MOVE 'Y'            TO  PI-ARCHIVE-RECEIVED
00521              MOVE REPLYI         TO  W-DEEDIT-FIELD
00522              PERFORM 8600-DEEDIT THRU 8600-EXIT
00523              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00524                  MOVE W-DEEDIT-FIELD-V0  TO  REPLYO
00525                  INSPECT REPLYI CONVERTING SPACES TO SLASH
00526                  MOVE '4'        TO  DC-OPTION-CODE
00527                  MOVE W-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
00528                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00529                  IF DC-ERROR-CODE NOT = SPACES
00530                      MOVE ER-9245        TO  EMI-ERROR
00531                      MOVE -1             TO  REPLYL
00532                      MOVE AL-UABON       TO  REPLYA
00533                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00534                  ELSE
00535                    IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
00536                        MOVE ER-0539      TO  EMI-ERROR
00537                        MOVE -1           TO  REPLYL
00538                        MOVE AL-UABON     TO  REPLYA
00539                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00540                    ELSE
00541                        MOVE AL-UANON     TO  REPLYA
00542                        MOVE +1           TO  W-UPDATE-SW
00543                        MOVE DC-BIN-DATE-1 TO W-RECEIVED-DATE
00544                        MOVE DC-GREG-DATE-1-EDIT TO 
00545                                         W-CERT-NOTE-RECV-DT
122612                                        WS-BN-RECV-DATE
00546                    END-IF
00547                  END-IF
00548              ELSE
00549                  MOVE ER-9245    TO  EMI-ERROR
00550                  MOVE -1         TO  REPLYL
00551                  MOVE AL-UABON   TO  REPLYA
00552                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00553              END-IF
00554          END-IF
00555      END-IF.
00556
00557      IF STOPDTEL GREATER ZERO
00558          IF STOPDTEI = SPACES
00559              MOVE AL-UANON       TO  STOPDTEA
00560              MOVE +1             TO  W-UPDATE-SW
00561              MOVE LOW-VALUES     TO  W-STOP-LETTER-DATE
00562              MOVE SPACES         TO  PI-ARCHIVE-STOPPED
00563              MOVE AL-UANOF       TO  RESENDA
00564                                      RESFORMA
00565                                      FINDATEA
00566                                      FINLACTA
00567                                      REPLYA
00568                                      COMMENTA
00569          ELSE
00570              MOVE 'Y'            TO  PI-ARCHIVE-STOPPED
00571              MOVE 'Y'            TO  W-NEED-COMMENT
00572              MOVE STOPDTEI       TO  W-DEEDIT-FIELD
00573              PERFORM 8600-DEEDIT THRU 8600-EXIT
00574              IF W-DEEDIT-FIELD-V0 IS NUMERIC
00575                  MOVE W-DEEDIT-FIELD-V0  TO  STOPDTEO
00576                  INSPECT STOPDTEI CONVERTING SPACES TO SLASH
00577                  MOVE '4'        TO  DC-OPTION-CODE
00578                  MOVE W-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY
00579                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00580                  IF DC-ERROR-CODE NOT = SPACES
00581                      MOVE ER-0897         TO  EMI-ERROR
00582                      MOVE -1              TO  STOPDTEL
00583                      MOVE AL-UABON        TO  STOPDTEA
00584                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00585                  ELSE
00586                    IF DC-BIN-DATE-1 GREATER THAN W-SAVE-BIN-DATE
00587                        MOVE ER-0895      TO  EMI-ERROR
00588                        MOVE -1           TO  STOPDTEL
00589                        MOVE AL-UABON     TO  STOPDTEA
00590                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00591                    ELSE
00592                      MOVE AL-UANON       TO  STOPDTEA
00593                      MOVE +1             TO  W-UPDATE-SW
00594                      MOVE DC-BIN-DATE-1  TO  W-STOP-LETTER-DATE
00595                    END-IF
00596                  END-IF
00597              ELSE
00598                  MOVE ER-0897    TO  EMI-ERROR
00599                  MOVE -1         TO  STOPDTEL
00600                  MOVE AL-UABON   TO  STOPDTEA
00601                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00602              END-IF
00603          END-IF
00604      END-IF.
00605
00606      IF W-NEED-COMMENT = 'Y'  AND 
00607         COMMENTL NOT > ZERO
00608            MOVE -1         TO  COMMENTL
00609            MOVE ER-7363    TO  EMI-ERROR
00610            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00611      END-IF.

102918     if (stopdtel > zeros)
102918        and (PI-ARCHIVE-STOPPED = 'Y')
102918        and (pi-initial-print-date <> low-values)
102918        if obvyni not = 'Y' AND 'N'
102918           move -1               to obvynl
102918           move al-uabon         to obvyna
102918           move er-7364          to emi-error
102918           perform 9900-error-format
102918                                 thru 9900-exit
102918        end-if
102918     end-if

102918     if obvynl > zeros
102918        and pi-archive-stopped <> 'Y'
102918        move -1                  to stopdtel
102918        move al-uabon            to stopdtea
102918        move er-7333             to emi-error
102918        perform 9900-error-format
102918                                 thru 9900-exit
102918     end-if

00613      IF COMMENTL GREATER ZERO
00614       OR  RESFORML GREATER ZERO
00615       OR  FINLACTL GREATER ZERO
102918      or obvynl > 0
00616          MOVE +1          TO  W-UPDATE-SW
00617      END-IF.
00618
00619      IF W-ERROR-COUNT GREATER ZERO
00620          GO TO 8200-SEND-DATAONLY
00621      END-IF.
00622
00623      IF W-UPDATE-SW NOT GREATER ZERO
00624          MOVE ER-3112    TO  EMI-ERROR
00625          MOVE -1         TO  MAINTL
00626          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00627          GO TO 8200-SEND-DATAONLY
00628      END-IF.
00629
00630      IF PI-ARCHIVE-STOPPED = 'Y'
00631         MOVE AL-SANOF       TO RESENDA
00632                                RESFORMA
00633                                FINDATEA
00634                                FINLACTA
00635                                REPLYA
102918*                              COMMENTA
00637      END-IF.
00638
00639      PERFORM 3000-READ-FOR-UPDATE THRU 3000-EXIT.
00640
00641      IF RESENDL GREATER ZERO
00642          MOVE W-RESEND-DATE      TO  LA-RESEND-DATE
00643      END-IF.
00644
00645      IF FINDATEL GREATER ZERO
00646          MOVE W-FINAL-ACT-DATE   TO  LA-FINAL-ACT-DATE
00647      END-IF.

00649      IF REPLYL GREATER ZERO  *> its really received date
00650         MOVE W-RECEIVED-DATE     TO LA-REPLY-DATE
00651         IF W-RECEIVED-DATE <> LOW-VALUES
102918           move w-cert-note-msg  to w-comment-line-1
102918           move +1               to w-comment-line-cnt
00652            PERFORM 4000-ADD-CERT-NOTE
                                       THRU 4099-EXIT
122612           PERFORM 4500-UPDATE-BILL-NOTE
                                       THRU 4500-EXIT
00653         END-IF
00654      END-IF

00656      IF RESFORML GREATER ZERO
00657         MOVE RESFORMI            TO  LA-RESEND-LETR
00658      END-IF.
00659
00660      IF FINLACTL GREATER ZERO
00661         MOVE FINLACTI            TO  LA-FINAL-ACT-IND
00662      END-IF.
00663
00664      IF STOPDTEL GREATER ZERO
00665         MOVE W-STOP-LETTER-DATE  TO  LA-VOIDED-DATE
00666      END-IF.

102918     perform 1620-read-elcert    thru 1620-exit
102918
102918     IF (OBVYNL > ZERO)
102918        and (obvyni <> la-void-onbase-yn)
102918        if (obvyni = 'N')
102918           and (la-void-onbase-yn = spaces)
102918           continue
102918        else
102918           if la-initial-print-date <> low-values
102918              perform 1600-update-obkeywords
102918                                 thru 1600-exit
102918           end-if
102918        end-if
102918        MOVE OBVYNI              TO LA-VOID-ONBASE-YN
102918     end-if
102918
102918     if (not cert-found)
102918        or (sql-update-failed)
102918        or ((la-initial-print-date = low-values)
102918              and
102918            (la-void-onbase-yn = 'Y'))
102918        exec cics unlock
102918           dataset (W-ARCH-FILE-ID)
102918        end-exec
102918        if connected-to-db
102918           move ' '              to ws-connect-sw
102918           display ' about to disconnect '
102918           exec sql
102918              disconnect
102918           end-exec
102918        end-if
102918     end-if
102918
102918     if not cert-found
102918        move er-0249             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if sql-update-failed
102918        move er-7331             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if (la-void-onbase-yn = 'Y')
102918        and (la-initial-print-date = low-values)
102918        move er-7334             to emi-error
102918        move -1                  to maintl
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     if sql-update-succeeded
102918        display ' about to commit work '
102918        exec sql
102918           commit work
102918        end-exec
102918     end-if
102918     if connected-to-db
102918        display ' about to disconnect all '
102918        exec sql
102918           disconnect
102918        end-exec
102918        move ' ' to ws-connect-sw
102918     end-if

00668      PERFORM 3100-REWRITE THRU 3100-EXIT.
00669
00670      IF COMMENTL NOT GREATER ZERO
00671
00672          MOVE ER-0000            TO  EMI-ERROR
00673          MOVE ' '                TO  MAINTO
00674          MOVE -1                 TO  MAINTL
00675          MOVE AL-UANOF           TO  MAINTA
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00677          GO TO 1000-SHOW
00678      END-IF.

102918     if (commentl > zeros)
102918        and (commenti not = spaces)
102918        move commenti            to w-cert-note-comment
102918        perform 4160-check-comment thru 4160-exit
102918        PERFORM 4000-ADD-CERT-NOTE
102918                                 THRU 4099-EXIT
102918     end-if

00680      PERFORM 3200-READ-ARCT-FOR-UPDATE THRU 3200-EXIT.
00681
00682      IF PI-COMMENT-INDEX EQUAL +20
00683          MOVE ER-0051    TO  EMI-ERROR
00684          MOVE -1         TO  MAINTL
00685          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00686          GO TO 8200-SEND-DATAONLY
00687      END-IF    
00688
00689      SET PI-COMMENT-INDEX UP BY +1.
00690      SET LC-NDX TO PI-COMMENT-INDEX.
00691      MOVE COMMENTI        TO  LT-COMMENT-LINE (LC-NDX)
00692      MOVE W-SAVE-BIN-DATE TO  LT-COMMENT-CHG-DT (LC-NDX).
00693      MOVE PI-PROCESSOR-ID TO  LT-COMMENT-CHG-BY (LC-NDX).
00694      ADD +1               TO  LT-NUM-LINES-ON-RECORD.
00695      
00696      
00697      IF W-ADD-ARCT = 'N'
00698          PERFORM 3300-REWRITE-ARCT THRU 3300-EXIT
00699      ELSE
00700          PERFORM 3350-INSERT-ARCT THRU 3350-EXIT
00701      END-IF.
00702
00703
00704      MOVE ER-0000                TO  EMI-ERROR.
00705      MOVE ' '                    TO  MAINTO.
00706      MOVE -1                     TO  MAINTL.
00707      MOVE AL-UANOF               TO  MAINTA.
00708      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00709      GO TO 1000-SHOW.
00710 
00711                                  EJECT
00712
00713  1000-SHOW.
00714 ***************************************************************
00715 *     THIS ROUTINE WILL READ THE ARCHIVE FILE WITH THE        *
00716 *     ARCHIVE NUMBER SPECIFIED FROM THE PRIOR SCREEN.         *
00717 ***************************************************************
00718      MOVE LOW-VALUES              TO W-ARCT-KEY.
00719      MOVE PI-COMPANY-CD           TO W-ARCH-COMPANY-CD
00720                                      W-ARCT-COMPANY-CD.
00721      MOVE PI-689-ARCHIVE-NUMBER   TO W-ARCH-NUMBER
00722                                      W-ARCT-ARCHIVE-NO.
00723      MOVE '3'                     TO W-ARCT-RECORD-TYPE.
00724      MOVE +0                      TO W-ARCT-LINE-SEQ-NO.
00725
00726      EXEC CICS HANDLE CONDITION
00727           NOTOPEN    (8010-ARCH-NOT-OPEN)
00728           NOTFND     (1070-ARCH-NOT-FOUND)
00729           ENDFILE    (1070-ARCH-NOT-FOUND)
00730      END-EXEC.
00731
00732      EXEC CICS READ
00733          DATASET (W-ARCH-FILE-ID)
00734          SET     (ADDRESS OF LETTER-ARCHIVE)
00735          RIDFLD  (W-ARCH-KEY)
00736      END-EXEC.
00737
00738      MOVE LA-FORM-A3             TO FORMNOO.
122612     MOVE LA-FORM-A3             TO PI-ARCHIVE-LTRID
00739      MOVE LA-ARCHIVE-NO          TO ARCHNOO.
00740      MOVE LA-CARRIER-A2          TO CARRIERO.
00741      MOVE LA-GROUPING-A2         TO GROUPO.
00742      MOVE LA-STATE-A2            TO STATEO.
00743      MOVE LA-ACCOUNT-A2          TO ACCTO.
00744      MOVE LA-CERT-PRIME-A2       TO CERTO.
00745      MOVE LA-CERT-SUFFIX-A2      TO SFXO.
121112
121112     IF LA-ENDT-ARCH-NO-X EQUAL LOW-VALUES OR SPACES
121112          MOVE SPACES            TO ENDARCHO
121112     ELSE
121112          MOVE LA-ENDT-ARCH-NO   TO ENDARCHO
121112     END-IF
00746
00747      IF  LA-CREATION-DATE EQUAL LOW-VALUES OR SPACES
00748          MOVE SPACES             TO CREATDTI
102918                                    pi-create-date
00749      ELSE
00750          MOVE LA-CREATION-DATE   TO DC-BIN-DATE-1
00751          MOVE ' '                TO DC-OPTION-CODE
00752          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00753          MOVE DC-GREG-DATE-1-EDIT TO CREATDTI
102918                                     pi-create-date
00754      END-IF.
00755
00756      MOVE LA-PROCESSOR-CD        TO CREATBYI.
00757
00758      IF  LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES OR SPACES
00759          MOVE SPACES             TO PRINTDTI
102918         move low-values         to pi-initial-print-date
00760      ELSE
102918         move la-initial-print-date
102918                                 to pi-initial-print-date
00761          MOVE LA-INITIAL-PRINT-DATE TO DC-BIN-DATE-1
00762          MOVE ' '                TO DC-OPTION-CODE
00763          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00764          MOVE DC-GREG-DATE-1-EDIT TO PRINTDTI
00765      END-IF.
00766
00767      IF  LA-RESEND-DATE = LOW-VALUES OR SPACES
00768          MOVE SPACES             TO RESENDI
00769      ELSE
00770          MOVE LA-RESEND-DATE     TO DC-BIN-DATE-1
00771          MOVE ' '                TO DC-OPTION-CODE
00772          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00773          MOVE DC-GREG-DATE-1-EDIT TO RESENDI
00774      END-IF.
00775      
00776      MOVE LA-RESEND-LETR         TO RESFORMI.
00777
00778      IF  LA-SENT-DATE = LOW-VALUES OR SPACES
00779          MOVE SPACES             TO RESPRNTI
00780          MOVE SPACES             TO PI-ARCHIVE-COMPLETE
00781      ELSE
00782          MOVE 'Y'                TO PI-ARCHIVE-COMPLETE
00783          MOVE LA-SENT-DATE     TO DC-BIN-DATE-1
00784          MOVE ' '                TO DC-OPTION-CODE
00785          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00786          MOVE DC-GREG-DATE-1-EDIT TO RESPRNTI
00787      END-IF.
00788
00789      IF  LA-FINAL-ACT-DATE EQUAL LOW-VALUES OR SPACES
00790          MOVE SPACES             TO FINDATEI
00791          MOVE SPACES             TO PI-ARCHIVE-FINAL
00792      ELSE
00793          MOVE 'Y'                TO PI-ARCHIVE-FINAL
00794          MOVE LA-FINAL-ACT-DATE  TO DC-BIN-DATE-1
00795          MOVE ' '                TO DC-OPTION-CODE
00796          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00797          MOVE DC-GREG-DATE-1-EDIT TO FINDATEI
00798      END-IF.
00799
00800      MOVE LA-FINAL-ACT-IND       TO FINLACTI.
00801
00802      IF  LA-REPLY-DATE = LOW-VALUES OR SPACES
00803          MOVE SPACES             TO REPLYI
00804          MOVE SPACES             TO PI-ARCHIVE-RECEIVED
00805      ELSE
00806          MOVE 'Y'                TO PI-ARCHIVE-RECEIVED
00807          MOVE LA-REPLY-DATE      TO DC-BIN-DATE-1
00808          MOVE ' '                TO DC-OPTION-CODE
00809          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00810          MOVE DC-GREG-DATE-1-EDIT TO REPLYI
00811      END-IF.
00812
00813      IF  LA-VOIDED-DATE = LOW-VALUES OR SPACES
00814          MOVE SPACES             TO STOPDTEI
00815          MOVE SPACES             TO PI-ARCHIVE-STOPPED
00816      ELSE
00817          MOVE 'Y'                TO PI-ARCHIVE-STOPPED
00818          MOVE LA-VOIDED-DATE     TO DC-BIN-DATE-1
00819          MOVE ' '                TO DC-OPTION-CODE
00820          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00821          MOVE DC-GREG-DATE-1-EDIT TO STOPDTEI
00822      END-IF.

102918     if LA-VOID-ONBASE-YN not = spaces
102918        move LA-VOID-ONBASE-YN   to obvyno
102918     end-if

102918     if la-onbase-unique-id numeric
102918        move la-onbase-unique-id to obuido
102918     end-if

00824      IF LA-LAST-MAINT-DATE = LOW-VALUES OR SPACES
00825          MOVE SPACES             TO MAINTDTI
00826      ELSE
00827          MOVE LA-LAST-MAINT-DATE TO DC-BIN-DATE-1
00828          MOVE ' '                TO DC-OPTION-CODE
00829          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
00830          MOVE DC-GREG-DATE-1-EDIT TO MAINTDTI
00831      END-IF.
00832
00833      IF LA-LAST-MAINT-TIMEX = LOW-VALUES OR SPACES
00834          MOVE ZEROES             TO MAINTTMO
00835      ELSE
00836          MOVE LA-LAST-MAINT-TIME TO W-TIME-IN
00837          MOVE W-TIME-OUT         TO MAINTTMO
00838      END-IF.
00839
00840      MOVE LA-LAST-UPDATED-BY     TO MAINTBYI.
00841
00842      IF (LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES)
00843        OR (LA-VOIDED-DATE  NOT EQUAL LOW-VALUES AND SPACES)
00844           MOVE AL-SANOF          TO RESENDA
00845                                     RESFORMA
00846                                     FINDATEA
00847                                     FINLACTA
00848                                     REPLYA
00849                                     COMMENTA
00850      END-IF.
00851
00852      IF LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES
00853          MOVE AL-SANOF           TO STOPDTEA
00854      END-IF.
00855
00856      EXEC CICS HANDLE CONDITION
00857           NOTOPEN    (8015-ARCT-NOT-OPEN)
00858           NOTFND     (1000-ARCT-NOT-FOUND)
00859           ENDFILE    (1000-ARCT-NOT-FOUND)
00860      END-EXEC.
00861
00862      EXEC CICS READ
00863          DATASET  (W-ARCT-FILE-ID)
00864          SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00865          RIDFLD   (W-ARCT-KEY)
00866      END-EXEC.
00867      
00868      SET LC-NDX TO W-ZEROS.
00869      PERFORM 20 TIMES
00870          SET LC-NDX UP BY +1
00871          IF LT-COMMENT-LINE (LC-NDX) > SPACES
00872              MOVE LT-COMMENT-LINE (LC-NDX) TO COMMENTI
00873              SET PI-COMMENT-INDEX TO LC-NDX
00874          END-IF
00875      END-PERFORM.
00876
00877      MOVE -1                     TO MAINTL.
00878      GO TO 8100-SEND-INITIAL-MAP.
00879
00880  1000-ARCT-NOT-FOUND.
00881      
00882      SET PI-COMMENT-INDEX        TO W-ZEROS.
00883      MOVE -1                     TO MAINTL.
00884      GO TO 8100-SEND-INITIAL-MAP.
00885
00886  1000-EXIT.
00887      EXIT.
00888
00889
00890  1070-ARCH-NOT-FOUND.
00891
00892      MOVE ER-7371                TO EMI-ERROR.
00893      MOVE -1                     TO ARCHNOL.
00894      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00895      GO TO 8100-SEND-INITIAL-MAP.
00896
100813
100813 1500-CHECK-Z-RECORD.
100813
100813     MOVE 'N' TO WS-Z-RECORD-IND
100813
100813     EXEC CICS STARTBR
100813          DATASET    ('ELLETR')
100813          RIDFLD     (ELLETR-KEY)
100813          GTEQ
100813          RESP      (W-RESPONSE)
100813     END-EXEC.
100813     IF NOT RESP-NORMAL
100813         GO TO 1500-ENDBR
100813     END-IF.
100813
100813 1500-READNEXT.
100813
100813     EXEC CICS READNEXT
100813         DATASET   ('ELLETR')
100813         INTO      (TEXT-FILES)
100813         RIDFLD    (ELLETR-KEY)
100813         RESP      (W-RESPONSE)
100813     END-EXEC
100813     IF RESP-NORMAL
100813        IF TX-CONTROL-PRIMARY(1:5) NOT = ELLETR-SAVE-PART-KEY
100813           GO TO 1500-ENDBR
100813        END-IF
100813
100813        IF TX-LINE-SQUEEZE-CONTROL = 'Z'
100813           MOVE 'Y' TO WS-Z-RECORD-IND
100813           GO TO 1500-ENDBR
100813        ELSE
100813           GO TO 1500-READNEXT
100813        END-IF
100813     ELSE
100813        GO TO 1500-ENDBR
100813     END-IF.
100813
100813 1500-ENDBR.
100813
100813     EXEC CICS ENDBR
100813         DATASET     ('ELLETR')
100813     END-EXEC.
100813
100813 1500-EXIT.
100813      EXIT.

102918 1600-update-obkeywords.
102918
102918     if not connected-to-db
102918        perform 1700-CONNECT-TO-DB
102918                                 thru 1700-exit
102918     end-if
102918
102918*    perform 1620-read-elcert    thru 1620-exit
102918*    if not cert-found
102918*       go to 1600-exit
102918*    end-if
102918
102918     if obvyni = 'N'
102918        if la-onbase-unique-id numeric
102918           and la-onbase-unique-id > 0
102918           perform 1610-void-void thru 1610-exit
102918           go to 1600-exit
102918        end-if
102918     end-if
102918
102918***  If I get here I assume they are voiding the letter
102918***  I am using a stored procedure so I can get back the
102918***  unique id from the insert and update the erarch
102918***  record with it.  May come in handy down the road
102918
102918     if obvyni = 'Y'
102918        and (la-onbase-unique-id not numeric
102918           or la-onbase-unique-id = zeros)
102918        continue
102918     else
102918        go to 1600-exit
102918     end-if
102918
102918     move 'CID Certs'            to db-doc-type
102918     move 'Status'               to db-key-word-type
102918     move 'V'                    to db-key-word-value
102918     move cm-state               to db-cert-state
102918     move cm-cert-prime          to db-cert-no
102918     move cm-lf-loan-expire-dt   to dc-bin-date-1
102918     if cm-ah-loan-expire-dt > cm-lf-loan-expire-dt
102918        move cm-ah-loan-expire-dt
102918                                 to dc-bin-date-1
102918     end-if
102918     move ' '                    to dc-option-code
102918     perform 9500-LINK-DATE-CONVERT thru 9500-exit
102918     if no-conversion-error
102918        move dc-greg-date-a-edit to db-cert-exp-dt
102918        inspect db-cert-exp-dt
102918           converting '/' to '-'
102918     end-if
102918     move la-initial-print-date  to dc-bin-date-1
102918     move ' '                    to dc-option-code
102918     perform 9500-LINK-DATE-CONVERT thru 9500-exit
102918     if no-conversion-error
102918        move dc-greg-date-a-edit to db-print-dt
102918        inspect db-print-dt
102918           converting '/' to '-'
102918     end-if
102918
102918     EXEC SQL
102918        CALL sp_OBKeyWordChgs_ADD
102918           @DocType       =  :db-doc-type,
102918           @CertNo        =  :db-cert-no,
102918           @PrintDt       =  :db-print-dt,
102918           @CertExpDt     =  :db-cert-exp-dt,
102918           @CertSt        =  :db-cert-state,
102918           @KeyWordType   =  :db-key-word-type,
102918           @KeyWordValue  =  :db-key-word-value,
102918           @RetValue      = :kd-unique-id OUT
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot INSERT row ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        set sql-update-failed to true
102918     else
102918        move kd-unique-id        to la-onbase-unique-id
102918                                    obuido
102918        set sql-update-succeeded to true
102918     end-if
102918
102918     .
102918 1600-exit.
102918     exit.
102918
102918 1610-void-void.
102918
102918     move la-onbase-unique-id    to kd-unique-id
102918     EXEC SQL
102918        SELECT
102918           RequestDate,
102918           KeyWordType,
102918           KeyWordValue,
102918           CompletionDate
102918        INTO
102918           :db-req-date,
102918           :db-key-word-type,
102918           :db-key-word-value,
102918           :db-complete-dt  :nu-Complete-date
102918        FROM
102918           OBKeyWordChgs
102918        where
102918           :kd-unique-id = UniqueID
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot read row ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        go to 1610-exit
102918     end-if
102918     if nu-complete-date = -1
102918        display ' complete date is null '
102918     end-if
102918     if nu-complete-date = -1  *> isnull
102918        and db-key-word-type = 'Status'
102918        and obvyni <> 'Y'
102918        and db-key-word-value = 'V'
102918
102918        EXEC SQL
102918           DELETE
102918              OBKeyWordChgs
102918           WHERE
102918              UniqueID = :kd-unique-id
102918        END-EXEC
102918
102918        if sqlcode = 0
102918           display ' delete succeeded '
102918           set sql-update-succeeded to true
102918           move zeros            to la-onbase-unique-id
102918        else
102918           display ' delete not successful ' sqlcode
102918           set sql-update-failed to true
102918        end-if
102918     end-if
102918
102918     .
102918 1610-exit.
102918     exit.
102918        
102918 1620-read-elcert.
102918
102918     move la-company-cd          to elcert-key
102918     move la-carrier-a2          to elcert-carrier
102918     move la-grouping-a2         to elcert-grouping
102918     move la-state-a2            to elcert-state
102918     move la-account-a2          to elcert-account
102918     move la-effect-date-a2      to elcert-eff-dt
102918     move la-cert-no-a2          to elcert-cert-no
102918
102918     exec cics read
102918        dataset    ('ELCERT')
102918        ridfld     (elcert-key)
102918        SET        (ADDRESS OF CERTIFICATE-MASTER)                    
102918        resp       (w-response)
102918     end-exec
102918
102918     if resp-normal
102918        set cert-found to true
102918     end-if
102918
102918     .
102918 1620-exit.
102918     exit.
102918
102918 1700-CONNECT-TO-DB.
102918
063022     move 'TEST_Logic'           to svr
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if
102918
102918     string
102918         usr delimited space
102918         "." delimited size
102918         pass delimited space into usr-pass
102918     end-string
102918
102918     EXEC SQL
102918        CONNECT TO :svr USER :usr-pass
102918     END-EXEC
102918
102918     if sqlcode not = 0
102918        move sqlcode             to ws-sql-code
102918        move ws-sql-code         to ws-dis-sql-code
102918        display ' Error: Cannot Connect  ' kd-unique-id
102918        display ' sql return code        ' ws-dis-sql-code
102918        display ' sql err mess           ' sqlerrmc
102918        move er-7332             to emi-error
102918        perform 9900-error-format thru 9900-exit
102918        go to 8200-send-dataonly
102918     end-if
102918
102918     set connected-to-db to true
102918     display ' good connection '
102918
102918     .
102918 1700-EXIT.
102918     EXIT.

00898  3000-READ-FOR-UPDATE.
00899
00900      EXEC CICS HANDLE CONDITION
00901           NOTOPEN    (8010-ARCH-NOT-OPEN)
00902           NOTFND     (1070-ARCH-NOT-FOUND)
00903           ENDFILE    (1070-ARCH-NOT-FOUND)
00904      END-EXEC.
00905
00906      MOVE PI-COMPANY-CD TO W-ARCH-COMPANY-CD
00907      MOVE PI-689-ARCHIVE-NUMBER TO W-ARCH-NUMBER
00908
00909      EXEC CICS READ
00910          DATASET (W-ARCH-FILE-ID)
00911          SET     (ADDRESS OF LETTER-ARCHIVE)
00912          RIDFLD  (W-ARCH-KEY)
00913          UPDATE
00914      END-EXEC.
00915
00916  3000-EXIT.
00917      EXIT.
00918
00919                                  EJECT
00920
00921  3100-REWRITE.
00922
00923      MOVE PI-PROCESSOR-ID        TO  LA-LAST-UPDATED-BY
00924                                      MAINTBYI.
00925      MOVE W-SAVE-BIN-DATE        TO  LA-LAST-MAINT-DATE.
00926      MOVE W-SAVE-DATE            TO  MAINTDTI.
00927
00928      MOVE EIBTIME                TO  LA-LAST-MAINT-TIME
00929                                      W-TIME-IN.
00930      MOVE W-TIME-OUT             TO  MAINTTMO.
00931
00932      EXEC CICS REWRITE
00933          DATASET (W-ARCH-FILE-ID)
00934          FROM    (LETTER-ARCHIVE)
00935      END-EXEC.
00936
00937  3100-EXIT.
00938      EXIT.
00939                                  EJECT
00940  3200-READ-ARCT-FOR-UPDATE.
00941
00942      EXEC CICS HANDLE CONDITION
00943           NOTFND     (3200-NOT-FOUND)
00944           ENDFILE    (3200-NOT-FOUND)
00945      END-EXEC.
00946
00947      MOVE PI-COMPANY-CD TO W-ARCT-COMPANY-CD.
00948      MOVE PI-689-ARCHIVE-NUMBER TO W-ARCT-ARCHIVE-NO.
00949      MOVE '3'                   TO W-ARCT-RECORD-TYPE.
00950      MOVE +0                    TO W-ARCT-LINE-SEQ-NO.
00951
00952      EXEC CICS READ
00953          DATASET  (W-ARCT-FILE-ID)
00954          SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00955          RIDFLD   (W-ARCT-KEY)
00956          UPDATE
00957      END-EXEC.
00958
00959      MOVE 'N' TO W-ADD-ARCT.
00960      GO TO 3200-EXIT.
00961
00962  3200-NOT-FOUND.
00963
00964      EXEC CICS GETMAIN
00965           SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
00966           LENGTH   (W-ARCT-LENGTH)
00967      END-EXEC.
00968
00969      MOVE 'Y'                    TO W-ADD-ARCT.
00970      MOVE LOW-VALUES             TO LETTER-ARCHIVE-TEXT.
00971      MOVE 'LT'                   TO LT-RECORD-ID.
00972      MOVE PI-COMPANY-CD          TO LT-COMPANY-CD.
00973      MOVE PI-689-ARCHIVE-NUMBER  TO LT-ARCHIVE-NO.
00974      MOVE '3'                    TO LT-RECORD-TYPE.
00975      MOVE +0                     TO LT-LINE-SEQ-NO.
00976      MOVE +0                     TO LT-NUM-LINES-ON-RECORD.
00977      SET PI-COMMENT-INDEX        TO W-ZEROS.
00978  
00979  3200-EXIT.
00980      EXIT.
00981
00982  3300-REWRITE-ARCT.
00983
00984      EXEC CICS REWRITE
00985          DATASET (W-ARCT-FILE-ID)
00986          FROM    (LETTER-ARCHIVE-TEXT)
00987      END-EXEC.
00988
00989  3300-EXIT.
00990      EXIT.
00991                                  EJECT
00992
00993  3350-INSERT-ARCT.
00994      EXEC CICS WRITE
00995           DATASET   (W-ARCT-FILE-ID)
00996           FROM      (LETTER-ARCHIVE-TEXT)
00997           RIDFLD    (LT-CONTROL-PRIMARY)
00998      END-EXEC.
00999
01000  3350-EXIT.
01001      EXIT.
01002                                  EJECT
01003
01004
01005  4000-ADD-CERT-NOTE.
01006  
01007      MOVE SPACES                 TO  ERCNOT-KEY                   
01008                                      ELCERT-KEY.                  
102918     move spaces                 to cert-note-records-holder
102918                                    ws-build-note-sw
102918                                    ws-ercnot-sw
102918     move +0                     to c1

01010      MOVE PI-COMPANY-CD          TO  ERCNOT-COMPANY-CD            
01011                                      ELCERT-COMPANY-CD.           
01012      MOVE PI-CARRIER             TO  ERCNOT-CARRIER               
01013                                      ELCERT-CARRIER.              
01014      MOVE PI-GROUPING            TO  ERCNOT-GROUPING              
01015                                      ELCERT-GROUPING.             
01016      MOVE PI-STATE               TO  ERCNOT-STATE                 
01017                                      ELCERT-STATE.                
01018      MOVE PI-ACCOUNT             TO  ERCNOT-ACCOUNT               
01019                                      ELCERT-ACCOUNT.              
01020      MOVE PI-CERT-EFF-DT         TO  ERCNOT-EFF-DT                
01021                                      ELCERT-EFF-DT.               
01022      MOVE PI-CERT-PRIME          TO  ERCNOT-CERT-PRIME            
01023                                      ELCERT-CERT-PRIME.           
01024      MOVE PI-CERT-SFX            TO  ERCNOT-CERT-SFX              
01025                                      ELCERT-CERT-SFX.             
01026      MOVE '1'                    TO  ERCNOT-REC-TYP               
01027      MOVE ZEROS                  TO  ERCNOT-SEQ.                  
01028      MOVE ERCNOT-KEY             TO  SV-PRIOR-KEY.                


102918     EXEC CICS STARTBR
102918        DATASET    ('ERCNOT')
102918        RIDFLD     (ercnot-key)
102918        GTEQ
102918        RESP       (W-RESPONSE)
102918     END-EXEC
102918
102918     IF RESP-NORMAL
102918        set ercnot-startbr to true
102918*       display ' resp normal startbr '
102918        perform until finished-with-notes
102918           EXEC CICS READNEXT
102918              DATASET    ('ERCNOT')
102918              RIDFLD     (ercnot-key)
102918              INTO       (cert-note-file)
102918              resp       (w-response)
102918           end-exec
102918           if (resp-normal)
102918              and (cz-control-primary (1:33) =
102918                 sv-prior-key (1:33))
102918              if cz-record-type = '1'
102918                 add +1 to c1
102918                 move cert-note-file
102918                                 to cert-note-record (c1)
102918              end-if
102918           else
102918              set finished-with-notes to true
102918           end-if
102918        end-perform
102918     end-if
102918
102918     if ercnot-startbr                        
102918*       display ' about to endbr ercnot '     
102918        exec cics endbr                       
102918           dataset    ('ERCNOT')              
102918        end-exec                              
102918     end-if                                   
102918     move c1                     to note-count
102918
102918     if c1 = +0
102918        perform 4100-add-note    thru 4100-exit
102918     else
102918        perform 4120-delete-cert-notes
102918                                 thru 4120-exit
102918        if resp-normal
102918           perform 4100-add-note thru 4100-exit
102918           if resp-normal
102918              perform 4140-put-back-cert-notes
102918                                 thru 4140-exit
102918              if resp-normal
102918                 display 'NOTE SUCCESSFULLY ADDED'
102918              else
102918                 display ' something wrong with put back '
102918              end-if
102918           else
102918              display ' something went wrong with adding note '
102918           end-if
102918        else
102918           display ' something went wrong with generic delete '
102918        end-if
102918     end-if
102918
102918     .
102918 4099-EXIT.
102918     EXIT.
102918
102918
102918 4100-add-note.
102918
102918***  Need to check how long the note is
102918
102918     if w-comment-line-cnt = +0
102918        go to 4100-exit
102918     end-if
102918
102918     move 'CZ'                to cert-note-file
102918     move sv-prior-key        to cz-control-primary
102918     move '1'                 to cz-record-type
102918     move +1                  to cz-note-sequence
102918     move w-save-bin-date     to cz-last-maint-dt
102918     move eibtime             to cz-last-maint-hhmmss
102918     move pi-processor-id     to cz-last-maint-user
102918     move w-comment-line-1    to cz-note
102918
102918     .
102918 4100-write.
102918
102918     exec cics write
102918        dataset   ('ERCNOT')
102918        from      (cert-note-file)
102918        ridfld    (cz-control-primary)
102918        resp      (w-response)
102918     end-exec
102918     if not resp-normal
102918        display ' error-ercnot-write ' w-response ' '
102918           cz-control-primary (2:33)
102918        go to 4100-exit
102918     else
102918        display 'NOTE SUCCESSFULLY ADDED'
102918     end-if
102918
102918     if w-comment-line-cnt > +1
102918        move w-comment-line-2    to cz-note
102918        add +1 to cz-note-sequence
102918        move +0                  to w-comment-line-cnt
102918        go to 4100-write
102918     end-if
102918
102918     .
102918 4100-exit.
102918     exit.
102918
102918
102918
102918 4120-delete-cert-notes.
102918 
102918     move sv-prior-key (1:33)    to ercnot-key
102918     move '1'                    to ercnot-rec-typ
102918     move +0                     to ercnot-seq
102918     exec cics delete
102918        dataset    ('ERCNOT')
102918        keylength  (ws-cert-note-generic-key-len)
102918        ridfld     (ercnot-key (1:34))
102918        generic
102918        resp       (w-response)
102918     end-exec
102918 
102918     .
102918 4120-exit.
102918     exit.
102918
102918 4140-put-back-cert-notes.
102918
102918     perform varying c1 from +1 by +1 until
102918        c1 > note-count
102918        move cert-note-record (c1)
102918                                 to cert-note-file
102918        add +2                   to cz-note-sequence
102918*       display ' about to write ' cz-control-primary (2:19) ' '
102918*          cz-control-primary (23:11) ' ' cz-note-sequence ' '
102918*           cz-record-type ' ' cz-note-information
102918        exec cics write
102918           dataset ('ERCNOT')
102918           FROM    (cert-note-file)
102918           ridfld  (cz-control-primary)
102918           resp    (w-response)
102918        end-exec
102918        if not resp-normal
102918           display ' error-ercnot-write subsequ ' w-response ' '
102918              cz-control-primary (2:33)
102918           move +999             to c1
102918        end-if
102918     end-perform
102918
102918    .
102918 4140-exit.
102918     exit.
102918
102918 4160-check-comment.
102918
102918     string
102918        w-cert-note-comment ' ' delimited by '  '
102918        la-form-a3 ' ' delimited by size
102918        la-processor-cd ' ' delimited by size
102918        pi-create-date delimited by size
102918           into w-stop-letter-comment
102918     end-string
102918
102918     perform varying n1 from +126 by -1 until
102918        w-stop-letter-comment(n1:1) <> space
102918     end-perform
102918     if n1 < +64
102918        move +1                  to w-comment-line-cnt
102918        move w-stop-letter-comment
102918                                 to w-comment-line-1
102918        go to 4160-exit
102918     end-if
102918
102918     move +2                     to w-comment-line-cnt
102918
102918     perform varying n1 from n1 by -1 until
102918        ((w-stop-letter-comment(n1:1) = ' ')
102918            and
102918         (n1 < +64))
102918        or (n1 < +1)
102918     end-perform
102918
102918     if n1 < +1
102918        move w-stop-letter-comment(1:63)
102918                                 to w-comment-line-1
102918        move w-stop-letter-comment(64:63)
102918                                 to w-comment-line-2
102918     else
102918        move w-stop-letter-comment(1:n1)
102918                                 to w-comment-line-1
102918        move w-stop-letter-comment(n1 + 1:126 - n1)
102918                                 to w-comment-line-2
102918     end-if
102918
102918     .
102918 4160-exit.
102918     exit.

122612 4500-UPDATE-BILL-NOTE SECTION.
122612
122612     EXEC CICS GETMAIN
122612          SET      (ADDRESS OF EOB-CODES)
122612          LENGTH   (ELEOBC-LENGTH)
122612     END-EXEC           
122612
122612     MOVE LOW-VALUES             TO ELEOBC-KEY
122612     MOVE PI-COMPANY-CD          TO EOBC-COMPANY-CD
122612     MOVE '5'                    TO EOBC-REC-TYPE
122612
122612     EXEC CICS STARTBR                                            
122612         DATASET   (ELEOBC-FILE-ID)
122612         RIDFLD    (ELEOBC-KEY)
122612         GTEQ
122612         RESP      (W-RESPONSE)
122612     END-EXEC
122612
122612     IF NOT RESP-NORMAL
122612        GO TO 4500-EXIT
122612     END-IF
122612      .
122612 4500-READNEXT-ELEOBC.
122612
122612     EXEC CICS READNEXT
122612        INTO    (EOB-CODES)
122612        DATASET (ELEOBC-FILE-ID)
122612        RIDFLD  (ELEOBC-KEY)
122612        RESP    (W-RESPONSE)
122612     END-EXEC
122612
122612     IF RESP-NORMAL
122612         IF EO-RECORD-TYPE NOT = '5'
122612             GO TO 4500-EXIT
122612         END-IF
122612     ELSE
122612         GO TO 4500-EXIT
122612     END-IF
122612     
122612     IF EO-RECORD-TYPE = '5' AND
122612        EO-EOB-CODE = PI-ARCHIVE-LTRID
122612           CONTINUE
122612     ELSE
122612         GO TO 4500-READNEXT-ELEOBC
122612     END-IF
122612     
122612     MOVE SPACES TO WS-FIND-BILLING-NOTE
122612     MOVE EO-DESCRIPTION TO WS-FBN-NOTE
122612     MOVE PI-ARCHIVE-LTRID TO WS-FBN-LTRID
122612
122612     EXEC CICS GETMAIN
122612          SET      (ADDRESS OF CERTIFICATE-NOTE)
122612          LENGTH   (ERNOTE-LENGTH)
122612     END-EXEC           
122612
122612     MOVE PI-COMPANY-CD          TO  ERNOTE-COMPANY-CD
122612     MOVE PI-CARRIER             TO  ERNOTE-CARRIER
122612     MOVE PI-GROUPING            TO  ERNOTE-GROUPING
122612     MOVE PI-STATE               TO  ERNOTE-STATE
122612     MOVE PI-ACCOUNT             TO  ERNOTE-ACCOUNT
122612     MOVE PI-CERT-EFF-DT         TO  ERNOTE-CERT-EFF-DT
122612     MOVE PI-CERT-PRIME          TO  ERNOTE-CERT-PRIME
122612     MOVE PI-CERT-SFX            TO  ERNOTE-CERT-SFX
041320     move '1'                    to  ernote-record-type
122612
122612     EXEC CICS READ
122612        DATASET    (ERNOTE-FILE-ID)
122612        RIDFLD     (ERNOTE-KEY)
122612        INTO       (CERTIFICATE-NOTE)
122612        RESP       (W-RESPONSE)
122612        UPDATE
122612     END-EXEC
122612
122612     IF RESP-NORMAL
122612       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
122612           (NOTE-SUB > +10) OR
122612           (CN-LINE (NOTE-SUB) (1:29) = 
122612                             WS-FIND-BILLING-NOTE (1:29))
122612       END-PERFORM
122612       IF CN-LINE (NOTE-SUB) (1:29) NOT = 
122612                              WS-FIND-BILLING-NOTE (1:29)
122612         EXEC CICS UNLOCK
122612            DATASET    (ERNOTE-FILE-ID)
122612         END-EXEC
122612         GO TO 4500-EXIT
122612       END-IF
122612
122612       MOVE WS-RECEIVED-NOTE TO CN-LINE (NOTE-SUB) (50:20)
122612       MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
122612       MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
122612       MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
122612       EXEC CICS REWRITE
122612          DATASET    (ERNOTE-FILE-ID)
122612          FROM       (CERTIFICATE-NOTE)
122612          RESP       (W-RESPONSE)
122612       END-EXEC
122612
122612       .
122612
122612 4500-EXIT.
122612     EXIT.
122612
01096      
01097  4800-CERTIFICATE-UPDATE SECTION.                                 
01098                                                                   
01099      EXEC CICS HANDLE CONDITION                                   
01100          NOTFND   (4899-EXIT)                                     
01101      END-EXEC.                                                    
01102                                                                   
01103      EXEC CICS READ                                               
01104      EQUAL                                                        
01105      DATASET   (ELCERT-FILE-ID)                                   
01106      SET       (ADDRESS OF CERTIFICATE-MASTER)                    
01107      RIDFLD    (ELCERT-KEY)                                       
01108      UPDATE     
01109      RESP      (W-RESPONSE)                                       
01110      END-EXEC.      
01111                                                                   
01112      IF RESP-NORMAL                                        
01113         IF CM-NOTE-SW = ' '                                    
01114            MOVE '1'        TO CM-NOTE-SW                       
01115         ELSE                                                   
01116            IF CM-NOTE-SW = '2'                                  
01117               MOVE '3'      TO CM-NOTE-SW                       
01118            ELSE                                                 
01119               IF CM-NOTE-SW = '4'                                
01120                   MOVE '5'    TO CM-NOTE-SW                       
01121               ELSE                                               
01122                  IF CM-NOTE-SW = '6'                              
01123                     MOVE '7'  TO CM-NOTE-SW                       
01124                  END-IF                                           
01125               END-IF                                             
01126            END-IF                                               
01127         END-IF                                                 
01128         EXEC CICS REWRITE                                            
01129            FROM      (CERTIFICATE-MASTER)                           
01130            DATASET   (ELCERT-FILE-ID)                               
01131         END-EXEC
01132      END-IF.                                                    
01133                                                                   
01134  4899-EXIT.                                                       
01135       EXIT.                                                       
01136                                                                   
01137
01138
01139  8010-ARCH-NOT-OPEN.
01140
01141      MOVE ER-7388                TO EMI-ERROR.
01142      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01143      MOVE -1                     TO MAINTL.
01144      GO TO 8200-SEND-DATAONLY.
01145
01146  8015-ARCT-NOT-OPEN.
01147
01148      MOVE ER-7373                TO EMI-ERROR.
01149      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01150      MOVE -1                     TO MAINTL.
01151      GO TO 8200-SEND-DATAONLY.
01152                                  EJECT
01153
01154  8100-SEND-INITIAL-MAP.
01155
01156      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
01157
01158      IF  NOT EMI-NO-ERRORS
01159          MOVE EMI-MESSAGE-AREA (1)
01160                                  TO ERRMSG1O
01161      ELSE
01162          MOVE SPACES             TO ERRMSG1O
01163      END-IF.
01164
01165      EXEC CICS SEND
01166          MAP    (W-MAP)
01167          MAPSET (W-MAPSET)
01168          FROM   (EL691AO)
01169          ERASE
01170          CURSOR
01171      END-EXEC.
01172
01173      GO TO 9000-RETURN-TRANS.
01174
01175  8100-EXIT.
01176      EXIT.
01177                                  EJECT
01178
01179  8200-SEND-DATAONLY.
01180
01181      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
01182
01183      IF  NOT EMI-NO-ERRORS
01184          MOVE EMI-MESSAGE-AREA (1)
01185                                  TO ERRMSG1O
01186      ELSE
01187          MOVE SPACES             TO ERRMSG1O
01188      END-IF.
01189
01190      EXEC CICS SEND
01191          MAP    (W-MAP)
01192          MAPSET (W-MAPSET)
01193          FROM   (EL691AO)
01194          DATAONLY
01195          CURSOR
01196      END-EXEC.
01197
01198      GO TO 9000-RETURN-TRANS.
01199
01200  8200-EXIT.
01201      EXIT.
01202                                  EJECT
01203
01204  8300-SEND-TEXT.
01205
01206      EXEC CICS SEND TEXT
01207          FROM    (LOGOFF-TEXT)
01208          LENGTH  (LOGOFF-LENGTH)
01209          ERASE
01210          FREEKB
01211      END-EXEC.
01212
01213      GO TO 9000-RETURN-TRANS.
01214
01215  8300-EXIT.
01216      EXIT.
01217                                  EJECT
01218
01219  8600-DEEDIT.
01220
01221      EXEC CICS BIF DEEDIT
01222           FIELD    (W-DEEDIT-FIELD)
01223           LENGTH   (15)
01224      END-EXEC.
01225
01226  8600-EXIT.
01227      EXIT.
01228                                  EJECT
01229
01230  9000-RETURN-TRANS.
01231
01232      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
01233      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO
01234      EXEC CICS RETURN
01235          TRANSID    (W-TRANSACTION)
01236          COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01237          LENGTH     (PI-COMM-LENGTH)
01238      END-EXEC.
01239
01240  9000-EXIT.
01241      EXIT.
01242                                  EJECT
01243
01244  9200-PA.
01245
01246      MOVE ER-7008                TO EMI-ERROR.
01247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01248      MOVE -1                     TO MAINTL.
01249      GO TO 8200-SEND-DATAONLY.
01250
01251  9200-EXIT.
01252      EXIT.
01253                                  EJECT
01254
01255  9300-DFHCLEAR.
01256
01257      MOVE PI-RETURN-TO-PROGRAM TO W-CALL-PGM.
01258      GO TO 9400-XCTL.
01259
01260  9300-EXIT.
01261      EXIT.
01262
01263  9400-XCTL.
01264
01265      EXEC CICS XCTL
01266          PROGRAM  (W-CALL-PGM)
01267          COMMAREA (PROGRAM-INTERFACE-BLOCK)
01268          LENGTH   (PI-COMM-LENGTH)
01269      END-EXEC.
01270
01271  9400-EXIT.
01272      EXIT.
01273                                  EJECT
01274
01275  9500-LINK-DATE-CONVERT.
01276
01277      EXEC CICS LINK
01278          PROGRAM    ('ELDATCV')
01279          COMMAREA   (DATE-CONVERSION-DATA)
01280          LENGTH     (DC-COMM-LENGTH)
01281      END-EXEC.
01282
01283  9500-EXIT.
01284      EXIT.
01285
01286  9600-FORMAT-DATE-TIME.
01287
01288      MOVE W-SAVE-DATE            TO RUNDTEO.
01289      MOVE EIBTIME                TO W-TIME-IN.
01290      MOVE W-TIME-OUT             TO RUNTIMEO.
01291      MOVE PI-COMPANY-ID          TO COMPANYO.
01292      MOVE PI-PROCESSOR-ID        TO USERIDO.
01293      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
01294
01295      IF PI-ARCHIVE-COMPLETE = 'Y'
01296         MOVE AL-SANOF           TO STCOMPA
01297         MOVE AL-SADOF           TO STRECVA
01298                                    STSTOPA
01299                                    STFINLA
01300      ELSE 
01301        IF PI-ARCHIVE-RECEIVED = 'Y'
01302            MOVE AL-SANOF        TO STRECVA
01303            MOVE AL-SADOF        TO STCOMPA
01304                                    STSTOPA
01305                                    STFINLA
01306        ELSE
01307          IF PI-ARCHIVE-STOPPED = 'Y'
01308              MOVE AL-SANOF      TO STSTOPA
01309              MOVE AL-SADOF      TO STCOMPA
01310                                    STRECVA
01311                                    STFINLA
01312          ELSE
01313            IF PI-ARCHIVE-FINAL = 'Y'
01314                MOVE AL-SANOF    TO STFINLA
01315                MOVE AL-SADOF    TO STCOMPA
01316                                    STRECVA
01317                                    STSTOPA
01318            ELSE
01319                MOVE AL-SADOF    TO STCOMPA
01320                                    STRECVA
01321                                    STSTOPA
01322                                    STFINLA
01323            END-IF
01324          END-IF
01325        END-IF
01326      END-IF.
01327                                    
01328
01329  9600-EXIT.
01330      EXIT.
01331                                  EJECT
01332
01333  9700-PGMID-ERROR.
01334
01335      EXEC CICS  HANDLE CONDITION
01336          PGMIDERR  (8300-SEND-TEXT)
01337      END-EXEC.
01338
01339      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
01340      MOVE ' '                    TO PI-ENTRY-CD-1.
01341      MOVE W-XCTL-005             TO W-CALL-PGM
01342                                     LOGOFF-PGM.
01343      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01344
01345      PERFORM 9400-XCTL THRU 9400-EXIT.
01346
01347  9700-EXIT.
01348      EXIT.
01349
01350  9800-ABEND.
01351
01352      MOVE W-LINK-004             TO W-CALL-PGM.
01353      MOVE DFHEIBLK               TO EMI-LINE1
01354
01355      EXEC CICS  LINK
01356          PROGRAM   (W-CALL-PGM)
01357          COMMAREA  (EMI-LINE1)
01358          LENGTH    (72)
01359      END-EXEC.
01360
01361      GO TO 8200-SEND-DATAONLY.
01362
01363  9800-EXIT.
01364      EXIT.
01365                                  EJECT
01366
01367  9900-ERROR-FORMAT.
01368
01369      IF  EMI-ERROR EQUAL ER-9097
01370          NEXT SENTENCE
01371      ELSE
01372          IF  EMI-ERRORS-COMPLETE
01373                  OR
01374              EMI-ERROR EQUAL W-LAST-ERROR
01375              GO TO 9900-EXIT
01376      END-IF.
01377
01378      ADD +1  TO  W-ERROR-COUNT.
01379      MOVE W-LINK-001             TO W-CALL-PGM.
01380
01381      EXEC CICS LINK
01382          PROGRAM    (W-CALL-PGM)
01383          COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01384          LENGTH     (EMI-COMM-LENGTH)
01385      END-EXEC.
01386
01387      MOVE EMI-ERROR              TO W-LAST-ERROR.
01388
01389  9900-EXIT.
01390      EXIT.
01391
01392  9905-INITIALIZE-SECURITY.
01393 ******************************************************************
01394 *                                                                *
01395 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
01396 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
01397 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
01398 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
01399 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
01400 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
01401 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
01402 *       ERROR CONDITION AND EXIT THE PROGRAM.                    *
01403 *                                                                *
01404 *       NOTE:  THE CARRIER/GRP/STATE/ACCOUNT SECURITY DATA       *
01405 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
01406 *                                                                *
01407 ******************************************************************
01408
01409      IF  PI-PROCESSOR-ID EQUAL 'LGXX'
01410          MOVE 'Y'                TO PI-DISPLAY-CAP
01411                                     PI-MODIFY-CAP
01412      ELSE
01413          EXEC CICS READQ TS
01414              QUEUE  (PI-SECURITY-TEMP-STORE-ID)
01415              INTO   (SECURITY-CONTROL)
01416              LENGTH (SC-COMM-LENGTH)
01417              ITEM   (1)
01418          END-EXEC
01419          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
01420                                  TO PI-DISPLAY-CAP
01421          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
01422                                  TO PI-MODIFY-CAP
01423      END-IF.
01424
01425  9905-EXIT.
01426                                  EJECT
01427
01428  9995-SECURITY-VIOLATION.
01429
01430      MOVE EIBDATE                TO SM-JUL-DATE.
01431      MOVE EIBTRMID               TO SM-TERMID.
01432      MOVE W-THIS-PGM             TO SM-PGM.
01433      MOVE EIBTIME                TO W-TIME-IN.
01434      MOVE W-TIME-OUT             TO SM-TIME.
01435      MOVE PI-PROCESSOR-ID        TO SM-PROCESSOR-ID.
01436
01437      EXEC CICS LINK
01438           PROGRAM  ('EL003')
01439           COMMAREA (SECURITY-MESSAGE)
01440           LENGTH   (80)
01441      END-EXEC.
01442
01443  9995-EXIT.
01444      EXIT.
01445
01446  9999-GOBACK.
01447
01448      GOBACK.
01449
01450  9999-EXIT.
01451      EXIT.
