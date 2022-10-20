00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS072A.                              
00009 *AUTHOR.     Pablo
00010 *            Colleyville, TX.
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS. THIS PROGRAM REPORTS THE RATES JOURNAL FILE
00026                                                                   
00027  ENVIRONMENT DIVISION.                                            
00028                                                                   
00029  INPUT-OUTPUT SECTION.                                            
00030                                                                   
00031  FILE-CONTROL.                                                    
00032                                                                   
00033      SELECT RT-JOURNAL-IN    ASSIGN TO SYS010.
00039      SELECT PRINTX           ASSIGN TO SYS008.
00040      SELECT DISK-DATE        ASSIGN TO SYS019.
00041      SELECT FICH             ASSIGN TO SYS020.

00044  DATA DIVISION.                                                   
00045                                                                   
00046  FILE SECTION.                                                    
00047                                                                   
00048  FD  RT-JOURNAL-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  RT-JOURNAL-IN-REC.
           05  filler                  pic x(34).
           05  rt-jp-rest-of-rec       pic x(2000).

00051                                                                   
00052  EJECT                                                            
00053  FD  PRINTX                                                       
00054                                  COPY ELCPRTFD.                   
00055  EJECT                                                            
00056  FD  FICH                                                         
00057                                  COPY ELCFCHFD.                   
00058  EJECT                                                            
00059  FD  DISK-DATE                                                    
00060                                  COPY ELCDTEFD.                   
00061  EJECT                                                            
00062  WORKING-STORAGE SECTION.                                         
00063  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00064  77  FILLER  PIC X(32) VALUE '********************************'.  
00065  77  FILLER  PIC X(32) VALUE '     ECS072A WORKING-STORAGE    '.  
00066  77  FILLER  PIC X(32) VALUE '*****VMOD=2.001*****************'.  
00067                                                                   
00068  77  ACTR                        PIC S99     COMP.                
00069  77  BCTR                        PIC S99     COMP.                
00070  77  CTR                         PIC S999    COMP.                
00071  77  CTS                         PIC S999    COMP.                
00072  77  CTT                         PIC S999    COMP.                
00073  77  WK-1                        PIC S999    COMP-3.              
00074  77  X                           PIC X.                           
00075  77  T-FLD                       PIC S9(6)V9(5)  COMP-3.          
00076  77  PAGER                       PIC S9(4)   VALUE +1.            
00077  77  LINE-CNT                    PIC S9(4)   VALUE ZEROS.         
00078  77  CTA                         PIC 9.                           
00079  77  MO-RTS                      PIC S999    COMP-3    VALUE +360.
       77  ws-records-processed        pic 9(5) value zeros.

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
                                                            
                                       COPY ERCRATE.
00081  01  WS-ABEND.                                                    
00082      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
00083      12  WS-ZERO                 PIC S9          VALUE ZERO.      
00084      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
00085      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
00086                                                                   
00087  01  WS.                                                          
00088      12  PGM-SUB                 PIC S999    COMP    VALUE +072.  
00089      12  RATE-FILE-STATUS        PIC XX          VALUE ZERO.      
00090  EJECT                                                            
00091      COPY ELCDTECX.                                               
00092  EJECT                                                            
00093      COPY ELCDTEVR.                                               
00094  EJECT                                                            
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

00095  01  DATA-LINE-1.                                                 
00096      12  FILLER                  PIC X       VALUE SPACE.         
00097      12  FILLER                  PIC X(6)    VALUE 'STATE '.      
00098      12  DL-1AA                  PIC XX.                          
00099      12  FILLER                  PIC XXX     VALUE ' - '.         
00100      12  DL-1A                   PIC X(30).                       
00101      12  FILLER                  PIC X(5)    VALUE SPACE.         
00102      12  DL-1DTE                 PIC X(28).                       
00103      12  FILLER                  PIC X(5)    VALUE SPACES.        
00104      12  DL-COMNT                PIC X(50).                       
00105                                                                   
00106  01  DATA-LINE-1A.                                                
00107      12  FILLER                  PIC X(11)   VALUE SPACES.        
00108      12  FILLER                  PIC X(8)    VALUE 'CLASS = '.    
00109      12  DL-CLASS                PIC XX.                          
00110      12  FILLER                  PIC X(16)                        
00111          VALUE '    DEVIATION = '.                                
00112      12  DL-DEV                  PIC XXX.                         
00113      12  FILLER                  PIC X(15)                        
00114          VALUE '    HIGH AGE = '.                                 
00115      12  DL-HI-AGE               PIC XX.                          
00116                                                                   
00117  01  DATA-LINE-2.                                                 
00118      12  FILLER                  PIC X(11)   VALUE '        *'.   
00119      12  DL-2-OVRD               PIC X(12).                       
00120      12  DL-2A                   PIC XX.                          
00121      12  FILLER                  PIC XXX     VALUE ' - '.         
00122      12  DL-2B                   PIC X(12).                       
00123      12  FILLER                  PIC X(5)    VALUE SPACES.        
00124      12  DL-2A-COMNT             PIC X(50).                       
00125                                                                   
00126  01  DATA-LINE-4.                                                 
00127      12  FILLER                  PIC X(11)   VALUE SPACES.        
00128      12  DL-4-OVRD               PIC X(06).                       
00129      12  FILLER      PIC X(11)                                    
00130          VALUE ' MORTALITY '.                                     
00131      12  FILLER                  PIC X(7)    VALUE 'CODE = '.     
00132      12  DL-4C                   PIC X(4)    VALUE SPACES.        
00133      12  FILLER                  PIC XXX     VALUE ' = '.         
00134      12  DL-4B                   PIC X(26).                       
00135                                                                   
00136  01  DATA-LINE-4A.                                                
00137      12  FILLER                  PIC X(11)   VALUE SPACES.        
00138      12  FILLER      PIC X(22)                                    
00139          VALUE 'MAXIMUM ATTAINED AGE '.                           
00140      12  DL-4AGE                 PIC Z9.                          
00141                                                                   
00142  01  DATA-LINE-5.                                                 
00143      12  FILLER                  PIC X(11)   VALUE SPACES.        
00144      12  FILLER      PIC X(29)                                    
00145          VALUE 'EXCEPTIONS     AGE'.                              
00146      12  DL-5A               OCCURS 8.                            
00147          16  DL-5B               PIC ZZ.                          
00148          16  DL-5C               PIC X(8).                        
00149                                                                   
00150  01  DATA-LINE-6.                                                 
00151      12  FILLER                  PIC X(26)   VALUE SPACES.        
00152      12  FILLER                  PIC X(13)   VALUE 'TERM'.        
00153      12  DL-6A               OCCURS 8.                            
00154          16  DL-6B               PIC ZZZ.                         
00155          16  DL-6C               PIC X(7).                        
00156                                                                   
00157  01  DATA-LINE-7.                                                 
00158      12  FILLER                  PIC X(26)   VALUE SPACES.        
00159      12  FILLER                  PIC X(9)    VALUE 'FACE'.        
00160      12  DL-7A               OCCURS 8.                            
00161          16  DL-7B               PIC Z(7).                        
00162          16  DL-7C               PIC X(3).                        
00163                                                                   
00164  01  DATA-LINE-8.                                                 
00165      12  FILLER                  PIC X(11)   VALUE SPACES.        
00166      12  DL-8-OVRD               PIC X(06).                       
00167      12  FILLER                  PIC X(17)                        
00168          VALUE ' RATES BY MONTHS'.                                
00169                                                                   
00170  01  DATA-LINE-9.                                                 
00171      12  FILLER                  PIC X(16)   VALUE SPACES.        
00172      12  FILLER                  PIC X(10)   VALUE '   1'.        
00173      12  FILLER                  PIC X(10)   VALUE '   2'.        
00174      12  FILLER                  PIC X(10)   VALUE '   3'.        
00175      12  FILLER                  PIC X(10)   VALUE '   4'.        
00176      12  FILLER                  PIC X(10)   VALUE '   5'.        
00177      12  FILLER                  PIC X(10)   VALUE '   6'.        
00178      12  FILLER                  PIC X(10)   VALUE '   7'.        
00179      12  FILLER                  PIC X(10)   VALUE '   8'.        
00180      12  FILLER                  PIC X(10)   VALUE '   9'.        
00181      12  FILLER                  PIC X(10)   VALUE '  10'.        
00182      12  FILLER                  PIC X(10)   VALUE '  11'.        
00183      12  FILLER                  PIC X(10)   VALUE '  12'.        
00184                                                                   
00185  01  DATA-LINE-10.                                                
00186      12  FILLER                  PIC X(5)    VALUE SPACES.        
00187      12  FILLER                  PIC X(4)    VALUE 'YEAR'.        
00188      12  DL-10A                  PIC ZZ9.                         
00189      12  DL-10B              OCCURS 12.                           
00190          16  DL-10C              PIC XX.                          
00191          16  DL-10D              PIC ZZ.Z(5).                     
00192      12  FILLER                  PIC XXXX    VALUE SPACES.        
00193                                                                   
00194  01  DATA-LINE-11.                                                
00195      12  FILLER                  PIC X(11)   VALUE '        *'.   
00196      12  DL-11-OVRD              PIC X(12).                       
00197      12  FILLER                  PIC X(5)    VALUE 'TYPE'.        
00198      12  DL-11A                  PIC XX.                          
00199      12  FILLER                  PIC XXX     VALUE ' - '.         
00200      12  DL-11B                  PIC X(12).                       
00201      12  FILLER                  PIC X(5)    VALUE SPACES.        
00202      12  DL-11A-COMM             PIC X(50).                       
00203                                                                   
00204  01  DATA-LINE-12.                                                
00205      12  FILLER                  PIC X(11)   VALUE SPACES.        
00206      12  FILLER                  PIC X(29)                        
00207          VALUE 'EXCEPTIONS     AGE'.                              
00208      12  DL-12A              OCCURS 8.                            
00209          16  DL-12B              PIC ZZ.                          
00210          16  DL-12C              PIC X(8).                        
00211                                                                   
00212  01  DATA-LINE-12A.                                               
00213      12  FILLER                  PIC X(26)   VALUE SPACES.        
00214      12  FILLER                  PIC X(13)   VALUE 'TERM'.        
00215      12  DL-12A-TERM     OCCURS 8.                                
00216          16  DL-12A-T            PIC ZZZ.                         
00217          16  DL-12A-FIL          PIC X(7).                        
00218                                                                   
00219                                                                   
00220  01  DATA-LINE-13.                                                
00221      12  FILLER                  PIC X(26)   VALUE SPACES.        
00222      12  FILLER                  PIC X(11)   VALUE 'BENEFIT'.     
00223      12  DL-13A              OCCURS 8.                            
00224          16  DL-13B              PIC Z(5).                        
00225          16  DL-13C              PIC X(5).                        
00226                                                                   
00227  01  DATA-LINE-13A.                                               
00228      12  FILLER                  PIC X(26)   VALUE SPACES.        
00229      12  FILLER                  PIC X(9)    VALUE 'FACE'.        
00230      12  DL-13A-BENF     OCCURS 8 TIMES.                          
00231          16  DL-13A-BF           PIC Z(7).                        
00232          16  DL-13A-FIL          PIC X(3).                        
00233                                                                   
00234  01  DATA-LINE-14.                                                
00235      12  FILLER                  PIC X(11)   VALUE SPACES.        
00236      12  DL-14-OVRD              PIC X(06).                       
00237      12  FILLER                  PIC X(18)                        
00238          VALUE ' RATES BY MONTHS'.                                
00239                                                                   
00240  01  TRAILER-LINE.                                                
00241      12  FILLER                  PIC X(110) VALUE SPACES.         
00242      12  TRAILER-DESC            PIC X(12)  VALUE SPACES.         
00243      12  TRAILER-DETL            PIC X(8)   VALUE SPACES.         
00244                                                                   
00245  EJECT                                                            
00246  01  HEAD-1.                                                      
00247      12  FILLER                  PIC X(45)   VALUE SPACES.        
00248      12  H1-OVRD-1               PIC X(12).                       
00249      12  FILLER                  PIC X(05)   VALUE ' AND '.       
00250      12  H1-OVRD-2               PIC X(12).                       
00251      12  FILLER                  PIC X(06)   VALUE ' RATES'.      
00252      12  FILLER                  PIC X(39)   VALUE SPACES.        
00253      12  FILLER                  PIC X(8)    VALUE 'ECS072 '.     
00254                                                                   
00255  01  HEAD-2.                                                      
00256      12  FILLER                  PIC X(47)   VALUE SPACES.        
00257      12  H1-A                    PIC X(30).                       
00258      12  FILLER                  PIC X(42)   VALUE SPACES.        
00259      12  H2-DATE                 PIC X(8).                        
00260                                                                   
00261  01  HEAD-2A.                                                     
00262      12  FILLER                  PIC X(53)   VALUE SPACES.        
00263      12  H2A-DATE                PIC X(18).                       
00264      12  FILLER                  PIC X(48)   VALUE SPACES.        
00265      12  FILLER                  PIC X(5)    VALUE 'PAGE '.       
00266      12  DL-1D                   PIC ZZ,ZZ9.                      
00267                                                                   
00268  EJECT                                                            
00269  01  PRETTY-DATE.                                                 
00270      12  PRETTY-MO               PIC Z9.                          
00271      12  FILLER                  PIC X       VALUE '-'.           
00272      12  PRETTY-DAY              PIC 99.                          
00273      12  FILLER                  PIC X       VALUE '-'.           
00274      12  PRETTY-YR               PIC 99.                          
00275                                                                   
00276  01  L1-CUR-DTE.                                                  
00277      12  FILLER      PIC X(27)                                    
00278          VALUE 'THESE ARE THE CURRENT RATES'.                     
00279                                                                   
00280  01  L1-DATER.                                                    
00281      12  FILLER                  PIC X(20)                        
00282          VALUE 'THESE RATES EXPIRED'.                             
00283      12  L1D-FILL                PIC X(8).                        

                                       COPY ELCDATE.

00286  PROCEDURE DIVISION.                                              
00287                                                                   
00288  0100-GET-CLAS-CARD.                                              
00289                                                                   
00290      COPY ELCDTERX.                                               
00291                                                                   
00292      MOVE COMPANY-NAME           TO H1-A.                         
00293      MOVE ALPH-DATE              TO H2A-DATE.                     
00294      MOVE CLAS-STARTM            TO CLAS-INDEXM.                  
00295                                                                   
00296  0110-SPACE-PRNT-ROUTINE.                                         
00297                                                                   
00298      MOVE 1                      TO CTR.                          
00299      PERFORM 0120-SPACER-RTN 8 TIMES.                             
00300      MOVE SPACE TO DL-10C (9) DL-10C (10) DL-10C (11) DL-10C (12).
00301      GO TO 0140-OPEN-FILES.                                       
00302                                                                   
00303  0120-SPACER-RTN.                                                 
00304                                                                   
00305      MOVE SPACES TO DL-5C (CTR)  DL-6C (CTR)  DL-7C (CTR).        
00306      MOVE SPACES TO DL-10C (CTR)  DL-12C (CTR)  DL-13C (CTR).     
00307      ADD 1 TO CTR.                                                
00308                                                                   
00309  EJECT                                                            
00310  0140-OPEN-FILES.                                                 
00311                                                                   
00312      OPEN INPUT RT-JOURNAL-IN
00313          OUTPUT PRINTX.                                           
00314                                                                   
00322      MOVE WS-CURRENT-DATE        TO H2-DATE.                      
00323      MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY.           
00324      MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD.                
00332                                                                   
00333  0150-READ-MASTER-RATE.                                           
00334                                                                   
00335      READ RT-JOURNAL-IN AT END
              GO TO 0510-EOJ-CLOSER
           END-READ

           MOVE rt-jp-rest-of-rec      TO JOURNAL-RECORD
           MOVE JP-RECORD-AREA         TO RATE-record

00340      IF RT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
00341         GO TO 0150-READ-MASTER-RATE
           END-IF

00352      COPY ELCRTEM1.                                               
00353                                                                   
           add 1 to ws-records-processed
00354      MOVE RT-ST-CODE             TO DL-1AA.                       
00355      MOVE RT-ST-DEV              TO DL-DEV.                       
00356      MOVE RT-STATE-CODE          TO STATE-L.                      
00357                                                                   
00358  0160-LOOK-UP-STATE.                                              
00359                              COPY ECSSTLOK.                       
00360                                                                   
00361      MOVE STATE-PIC (CLAS-INDEXS) TO DL-1A.                       
00362                                                                   
00363      IF RT-EXP-DA = 99                                            
00364          MOVE L1-CUR-DTE         TO DL-1DTE                       
00365      ELSE                                                         
00366          MOVE RT-EXP-YR          TO PRETTY-YR                     
00367          MOVE RT-EXP-MO          TO PRETTY-MO                     
00368          MOVE RT-EXP-DA          TO PRETTY-DAY                    
00369          MOVE PRETTY-DATE        TO L1D-FILL                      
00370          MOVE L1-DATER           TO DL-1DTE.                      
00371                                                                   
00372      MOVE PAGER                  TO DL-1D.                        
00373      ADD 1 TO PAGER.                                              
00374      MOVE ZEROS                  TO LINE-CNT.                     
00375      MOVE '1'                    TO X.                            
00376      MOVE LIFE-OVERRIDE-L12      TO H1-OVRD-1.                    
00377      MOVE   AH-OVERRIDE-L12      TO H1-OVRD-2.                    
00378      MOVE HEAD-1                 TO P-DATA.                       
00379      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00380      MOVE HEAD-2                 TO P-DATA.                       
00381      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00382      MOVE HEAD-2A                TO P-DATA.                       
00383      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00384      MOVE '0'                    TO X.                            

           move jp-user-id             to dl0-user
           move jp-program-id          to dl0-program
           move jp-time                to dl0-time
           move jp-date                to dc-julian-date
           move '5'                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to dl0-date
           else
              move 'xx/xx/xxxx'        to dl0-date
           end-if
           evaluate jp-record-type
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
00385                                                                   
00386      MOVE RT-ST-CLASS            TO DL-CLASS.                     
00387      MOVE RT-HIGH-AGE            TO DL-HI-AGE.                    
00388      MOVE RT-STRUCTURE-COMMENT   TO DL-COMNT.                     
00389                                                                   
00390      MOVE DATA-LINE-1            TO P-DATA.                       
00391      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00392      MOVE DATA-LINE-1A           TO P-DATA.                       
00393      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00394      MOVE '0'                    TO X.                            
00395      IF RT-L-AH NOT = 'L'                                         
00396          GO TO 0220-DOING-A-AND-H.                                
00397                                                                   
00398  0170-PRINT-THIS-LIFE.                                            
00399                                                                   
00400      MOVE RT-LAH-NUM             TO DL-2A.                        
00401      MOVE LIFE-OVERRIDE-L12      TO DL-2-OVRD.                    
00402      PERFORM 0440-FIND-L-NAME THRU 0460-E-F-L-NAME.               
00403      MOVE RT-RATE-COMMENT        TO DL-2A-COMNT.                  
00404      MOVE DATA-LINE-2            TO P-DATA.                       
00405      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00406      MOVE SPACES                 TO DL-4B.                        
00407                                                                   
00408      IF RT-LIFE-MORT-CODE NOT = CLAS-MORT-CODE (CLAS-INDEXM)      
00409          MOVE CLAS-STARTM        TO CLAS-INDEXM.                  
00410                                                                   
00411  0180-MORT-DESC-LOOP.                                             
00412                                                                   
00413      IF RT-LIFE-MORT-CODE = CLAS-MORT-CODE (CLAS-INDEXM)          
00414          MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO DL-4B               
00415      ELSE                                                         
00416          ADD +1 TO CLAS-INDEXM                                    
00417          IF CLAS-INDEXM NOT GREATER THAN CLAS-MAXM                
00418              GO TO 0180-MORT-DESC-LOOP.                           
00419                                                                   
00420      MOVE LIFE-OVERRIDE-L6       TO DL-4-OVRD.                    
00421      MOVE RT-LIFE-MORT-CODE      TO DL-4C.                        
00422      MOVE DATA-LINE-4            TO P-DATA.                       
00423      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00424      MOVE RT-MAX-AGE             TO DL-4AGE.                      
00425      MOVE DATA-LINE-4A           TO P-DATA.                       
00426      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00427      MOVE 1                      TO CTS.                          
00428      PERFORM 0320-L-X-AGE 8 TIMES.                                
00429      MOVE DATA-LINE-5            TO P-DATA.                       
00430      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00431      MOVE 1                      TO CTS.                          
00432      PERFORM 0340-L-X-TERM 8 TIMES.                               
00433      MOVE DATA-LINE-6            TO P-DATA.                       
00434      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00435      MOVE 1                      TO CTS.                          
00436      PERFORM 0350-L-X-FACE 8 TIMES.                               
00437      MOVE DATA-LINE-7            TO P-DATA.                       
00438      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00439      MOVE '-'                    TO X.                            
00440      MOVE LIFE-OVERRIDE-L6       TO DL-8-OVRD.                    
00441      MOVE DATA-LINE-8            TO P-DATA.                       
00442      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00443      MOVE '0'                    TO X.                            
00444      MOVE DATA-LINE-9            TO P-DATA.                       
00445      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00446      MOVE 1                      TO CTT.                          
00447                                                                   
00448  0190-CALC-YEAR-NUMBER-L.                                         
00449                                                                   
00450      COMPUTE WK-1 = (CTT + 11) / 12.                              
00451      MOVE WK-1                   TO DL-10A.                       
00452      MOVE CTT                    TO CTS.                          
00453      MOVE ZERO                   TO T-FLD.                        
00454      PERFORM 0330-LIFE-RATE-ADD 12 TIMES.                         
00455                                                                   
00456      IF T-FLD = ZERO                                              
00457          ADD 12 TO CTT                                            
00458          GO TO 0200-CK-LIFE-END.                                  
00459                                                                   
00460      MOVE 1                      TO CTS.                          
00461      PERFORM 0360-FILL-LIFE-RATES 12 TIMES.                       
00462      MOVE DATA-LINE-10           TO P-DATA.                       
00463      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00464                                                                   
00465  0200-CK-LIFE-END.                                                
00466                                                                   
00467      IF CTT NOT GREATER MO-RTS                                    
00468          GO TO 0190-CALC-YEAR-NUMBER-L.                           
00469                                                                   
00470      PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    
00471      GO TO 0150-READ-MASTER-RATE.                                 
00472                                                                   
00473  EJECT                                                            
00474  0220-DOING-A-AND-H.                                              
00475                                                                   
00476      MOVE AH-OVERRIDE-L12        TO DL-11-OVRD.                   
00477      MOVE RT-LAH-NUM             TO DL-11A.                       
00478      PERFORM 0470-FIND-D-NAME THRU 0490-E-F-D-NAME.               
00479      MOVE RT-RATE-COMMENT        TO DL-11A-COMM.                  
00480      MOVE DATA-LINE-11           TO P-DATA.                       
00481      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00482      MOVE RT-MAX-AGE             TO DL-4AGE.                      
00483      MOVE DATA-LINE-4A           TO P-DATA.                       
00484      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00485      MOVE 1                      TO CTS.                          
00486      PERFORM 0380-AH-X-AGE 8 TIMES.                               
00487      MOVE DATA-LINE-12           TO P-DATA.                       
00488      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00489      MOVE 1 TO CTS.                                               
00490      PERFORM 0390-AH-X-TERM 8 TIMES.                              
00491      MOVE DATA-LINE-12A          TO P-DATA.                       
00492      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00493      MOVE 1                      TO CTS.                          
00494      PERFORM 0400-AH-X-BEN 8 TIMES.                               
00495      MOVE DATA-LINE-13           TO P-DATA.                       
00496      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00497      MOVE 1 TO CTS.                                               
00498      PERFORM 0410-AH-X-FACE 8 TIMES.                              
00499      MOVE DATA-LINE-13A          TO P-DATA.                       
00500      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00501      MOVE '-'                    TO X.                            
00502      MOVE AH-OVERRIDE-L6         TO DL-14-OVRD.                   
00503      MOVE DATA-LINE-14           TO P-DATA.                       
00504      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00505      MOVE '0'                    TO X.                            
00506      MOVE DATA-LINE-9            TO P-DATA.                       
00507      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00508      MOVE 1                      TO CTT.                          
00509                                                                   
00510  0230-CALC-YEAR-NUMBER-AH.                                        
00511                                                                   
00512      COMPUTE WK-1 = (CTT + 11) / 12.                              
00513      MOVE WK-1                   TO DL-10A.                       
00514      MOVE CTT                    TO CTS.                          
00515      MOVE ZERO                   TO T-FLD.                        
00516      PERFORM 0370-AH-RATE-ADD 12 TIMES.                           
00517                                                                   
00518      IF T-FLD = ZERO                                              
00519          ADD 12 TO CTT                                            
00520          GO TO 0240-CK-AH-END.                                    
00521                                                                   
00522      MOVE 1                      TO CTS.                          
00523      PERFORM 0420-FILL-AH-RATES 12 TIMES.                         
00524      MOVE DATA-LINE-10           TO P-DATA.                       
00525      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00526                                                                   
00527  0240-CK-AH-END.                                                  
00528                                                                   
00529      IF CTT NOT GREATER MO-RTS                                    
00530          GO TO 0230-CALC-YEAR-NUMBER-AH.                          
00531                                                                   
00532      PERFORM 0290-TRAILER-ROUTINE THRU 0300-TRAILER-ROUTINE-X.    
00533      GO TO 0150-READ-MASTER-RATE.                                 
00534                                                                   
00535  EJECT                                                            
00536  0260-PRT-RTN.                                                    
00537                                  COPY ELCPRT2.                    
00538      ADD 1 TO LINE-CNT.                                           
00539                                                                   
00540      IF X = '0'                                                   
00541          ADD 1 TO LINE-CNT                                        
00542      ELSE                                                         
00543          IF X = '-'                                               
00544              ADD 2 TO LINE-CNT.                                   
00545                                                                   
00546      MOVE ' '                    TO X.                            
00547                                                                   
00548  0270-PRT-RTN-EXIT.    EXIT.                                      
00549                                                                   
00550  EJECT                                                            
00551  0290-TRAILER-ROUTINE.                                            
00552                                                                   
00553      IF LINE-CNT LESS 48                                          
00554          MOVE SPACES             TO P-DATA                        
00555          PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT              
00556          GO TO 0290-TRAILER-ROUTINE.                              
00557                                                                   
00558      MOVE 'STATE'                TO TRAILER-DESC.                 
00559      MOVE RT-ST-CODE             TO TRAILER-DETL.                 
00560      MOVE TRAILER-LINE           TO P-DATA.                       
00561      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00562      MOVE 'CLASS'                TO TRAILER-DESC.                 
00563      MOVE RT-ST-CLASS            TO TRAILER-DETL.                 
00564      MOVE TRAILER-LINE           TO P-DATA.                       
00565      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00566      MOVE 'DEVIATION'            TO TRAILER-DESC.                 
00567      MOVE RT-ST-DEV              TO TRAILER-DETL.                 
00568      MOVE TRAILER-LINE           TO P-DATA.                       
00569      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00570      MOVE 'TYPE'                 TO TRAILER-DESC.                 
00571      MOVE RT-L-AH-CODE           TO TRAILER-DETL.                 
00572      MOVE TRAILER-LINE           TO P-DATA.                       
00573      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00574      MOVE 'HIGH AGE'             TO TRAILER-DESC.                 
00575      MOVE RT-HIGH-AGE            TO TRAILER-DETL.                 
00576      MOVE TRAILER-LINE           TO P-DATA.                       
00577      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00578      MOVE RT-EXP-YR              TO PRETTY-YR.                    
00579      MOVE RT-EXP-MO              TO PRETTY-MO.                    
00580      MOVE RT-EXP-DA              TO PRETTY-DAY.                   
00581      MOVE PRETTY-DATE            TO TRAILER-DETL.                 
00582      MOVE 'EXP DATE'             TO TRAILER-DESC.                 
00583      MOVE TRAILER-LINE           TO P-DATA.                       
00584      PERFORM 0260-PRT-RTN THRU 0270-PRT-RTN-EXIT.                 
00585                                                                   
00586  0300-TRAILER-ROUTINE-X.     EXIT.                                
00587                                                                   
00588  EJECT                                                            
00589  0320-L-X-AGE.                                                    
00590                                                                   
00591      MOVE RT-L-EX-AGE (CTS)      TO DL-5B (CTS).                  
00592      ADD 1 TO CTS.                                                
00593                                                                   
00594  0330-LIFE-RATE-ADD.                                              
00595                                                                   
00596      ADD RT-L-RATE (CTS)         TO T-FLD.                        
00597      ADD 1 TO CTS.                                                
00598                                                                   
00599  0340-L-X-TERM.                                                   
00600                                                                   
00601      MOVE RT-L-EX-TERM (CTS)     TO DL-6B (CTS).                  
00602      ADD 1 TO CTS.                                                
00603                                                                   
00604  0350-L-X-FACE.                                                   
00605                                                                   
00606      MOVE RT-L-EX-FACE (CTS)     TO DL-7B (CTS).                  
00607      ADD 1 TO CTS.                                                
00608                                                                   
00609  0360-FILL-LIFE-RATES.                                            
00610                                                                   
00611      MOVE RT-L-RATE (CTT)        TO DL-10D (CTS).                 
00612      ADD 1 TO CTT.                                                
00613      ADD 1 TO CTS.                                                
00614                                                                   
00615  0370-AH-RATE-ADD.                                                
00616                                                                   
00617      ADD RT-AH-RATE (CTS)        TO T-FLD.                        
00618      ADD 1 TO CTS.                                                
00619                                                                   
00620  0380-AH-X-AGE.                                                   
00621                                                                   
00622      MOVE RT-AH-AGE (CTS)        TO DL-12B (CTS).                 
00623      ADD 1 TO CTS.                                                
00624                                                                   
00625  0390-AH-X-TERM.                                                  
00626                                                                   
00627      MOVE RT-AH-TERM (CTS)       TO DL-12A-T (CTS).               
00628      MOVE SPACES                 TO DL-12A-FIL (CTS).             
00629      ADD 1 TO CTS.                                                
00630                                                                   
00631  0400-AH-X-BEN.                                                   
00632                                                                   
00633      MOVE RT-AH-BEN-M (CTS)      TO DL-13B (CTS).                 
00634      ADD 1 TO CTS.                                                
00635                                                                   
00636  0410-AH-X-FACE.                                                  
00637                                                                   
00638      MOVE RT-AH-BEN-F (CTS)      TO DL-13A-BF (CTS).              
00639      MOVE SPACES                 TO DL-13A-FIL (CTS).             
00640      ADD 1 TO CTS.                                                
00641                                                                   
00642  0420-FILL-AH-RATES.                                              
00643                                                                   
00644      MOVE RT-AH-RATE (CTT)       TO DL-10D (CTS).                 
00645      ADD 1 TO CTT.                                                
00646      ADD 1 TO CTS.                                                
00647                                                                   
00648  EJECT                                                            
00649  0440-FIND-L-NAME.                                                
00650                                                                   
00651      MOVE SPACES                 TO DL-2B.                        
00652                                                                   
00653      IF CLAS-STARTL = ZERO                                        
00654          GO TO 0460-E-F-L-NAME.                                   
00655                                                                   
00656      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
00657                                                                   
00658  0450-FLN-LOOP.                                                   
00659                                                                   
00660      IF CLAS-I-BEN (CLAS-INDEXL) = RT-LAH-NUM                     
00661          MOVE CLAS-I-AB10 (CLAS-INDEXL) TO DL-2B                  
00662          GO TO 0460-E-F-L-NAME.                                   
00663                                                                   
00664      ADD +1 TO CLAS-INDEXL.                                       
00665                                                                   
00666      IF CLAS-INDEXL NOT GREATER THAN CLAS-MAXL                    
00667          GO TO 0450-FLN-LOOP.                                     
00668                                                                   
00669  0460-E-F-L-NAME.     EXIT.                                       
00670                                                                   
00671                                                                   
00672  0470-FIND-D-NAME.                                                
00673                                                                   
00674      MOVE SPACES                 TO DL-11B.                       
00675                                                                   
00676      IF CLAS-STARTA = ZERO                                        
00677          GO TO 0490-E-F-D-NAME.                                   
00678                                                                   
00679      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
00680                                                                   
00681  0480-FDN-LOOP.                                                   
00682                                                                   
00683      IF CLAS-I-BEN (CLAS-INDEXA) = RT-LAH-NUM                     
00684          MOVE CLAS-I-AB10 (CLAS-INDEXA) TO DL-11B                 
00685          GO TO 0490-E-F-D-NAME.                                   
00686                                                                   
00687      ADD +1 TO CLAS-INDEXA.                                       
00688                                                                   
00689      IF CLAS-INDEXA NOT GREATER THAN CLAS-MAXA                    
00690          GO TO 0480-FDN-LOOP.                                     
00691                                                                   
00692  0490-E-F-D-NAME.     EXIT.                                       
00693                                                                   
00694  EJECT                                                            
00695  0510-EOJ-CLOSER.                                                 

           if ws-records-processed = zeros
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

00696                              COPY ELCPRTC.                        
00697                                                                   
00698      CLOSE rt-journal-in
00699            PRINTX.                                                
00700                                                                   
00701      GOBACK.                                                      

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

00703  ABEND-PGM SECTION.              COPY ELCABEND.                   
