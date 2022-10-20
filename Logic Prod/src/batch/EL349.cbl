00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL349 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/20/95 12:50:51.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.022                           
00009                                                                   
00009                                                                   
00010 *AUTHOR.     LOGIC INC.                                           
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022                                                                   
00023 *REMARKS.                                                         
00024 **************** THIS PROGRAM RUNS FOR ALL CLIENTS -              
00025 **                              (NOT DATE-CARD DRIVEN)            
00026 **                                                                
00027 *         THIS PROGRAM IS USED TO CREATE AND DELETE DUMMY ENTRIES 
00028 *       IN ALL CLAS-IC VSAM FILES.  THIS WILL THEN ALLOW ACCESS   
00029 *       TO NULL FILES VIA CICS.                                   
00030                                                                   
00031  ENVIRONMENT DIVISION.                                            
00032  INPUT-OUTPUT SECTION.                                            
00033  FILE-CONTROL.                                                    
00034                                                                   
00035      SELECT CARD-FILE     ASSIGN TO SYS006-UR-3505-S-SYS006.      
00036                                                                   
00037      SELECT ELACTQ        ASSIGN TO SYS010-FBA1-ELACTQ            
00038                           ORGANIZATION    INDEXED                 
00039                           ACCESS          DYNAMIC                 
00040                           RECORD KEY      AQ-CONTROL-PRIMARY      
00041                                           IN ELACTQ               
00042                           FILE STATUS     CHK.                    
00043                                                                   
00044      SELECT ELALPH        ASSIGN TO SYS010-FBA1-ELALPH            
00045                           ORGANIZATION    INDEXED                 
00046                           ACCESS          DYNAMIC                 
00047                           RECORD KEY      AI-CONTROL-PRIMARY      
00048                                           IN ELALPH               
00049                           FILE STATUS     CHK.                    
00050                                                                   
00051      SELECT ELARCH        ASSIGN TO SYS010-FBA1-ELARCH            
00052                           ORGANIZATION    INDEXED                 
00053                           ACCESS          DYNAMIC                 
00054                           RECORD KEY      LA-CONTROL-PRIMARY      
00055                                           IN ELARCH               
00056                           FILE STATUS     CHK.                    
00057                                                                   
00058      SELECT ELARCT        ASSIGN TO SYS010-FBA1-ELARCT            
00059                           ORGANIZATION    INDEXED                 
00060                           ACCESS          DYNAMIC                 
00061                           RECORD KEY      LT-CONTROL-PRIMARY      
00062                                           IN ELARCT               
00063                           FILE STATUS     CHK.                    
00064                                                                   
00065      SELECT ELBENE        ASSIGN TO SYS010-FBA1-ELBENE            
00066                           ORGANIZATION    INDEXED                 
00067                           ACCESS          DYNAMIC                 
00068                           RECORD KEY      BE-CONTROL-PRIMARY      
00069                                           IN ELBENE               
00070                           FILE STATUS     CHK.                    
00071                                                                   
00072      SELECT ELCERT        ASSIGN TO SYS010-FBA1-ELCERT            
00073                           ORGANIZATION    INDEXED                 
00074                           ACCESS          DYNAMIC                 
00075                           RECORD KEY      CM-CONTROL-PRIMARY      
00076                                           IN ELCERT               
00077                           FILE STATUS     CHK.                    
00078                                                                   
00072      SELECT ELPURG        ASSIGN TO SYS010-FBA1-ELPURG            
00073                           ORGANIZATION    INDEXED                 
00074                           ACCESS          DYNAMIC                 
00075                           RECORD KEY      PG-CONTROL-PRIMARY      
00076                                           IN ELPURG               
00077                           FILE STATUS     CHK.                    
00078                                                                   
00079      SELECT ELCHKQ        ASSIGN TO SYS010-FBA1-ELCHKQ            
00080                           ORGANIZATION    INDEXED                 
00081                           ACCESS          DYNAMIC                 
00082                           RECORD KEY      CQ-CONTROL-PRIMARY      
00083                                           IN ELCHKQ               
00084                           FILE STATUS     CHK.                    
00085                                                                   
00086      SELECT ELCNTL        ASSIGN TO SYS010-FBA1-ELCNTL            
00087                           ORGANIZATION    INDEXED                 
00088                           ACCESS          DYNAMIC                 
00089                           RECORD KEY      CF-CONTROL-PRIMARY      
00090                                           IN ELCNTL               
00091                           FILE STATUS     CHK.                    
00092                                                                   
00093      SELECT ELRCON        ASSIGN TO SYS010-FBA1-ELRCON            
00094                           ORGANIZATION    INDEXED                 
00095                           ACCESS          DYNAMIC                 
00096                           RECORD KEY      RC-CONTROL-PRIMARY      
00097                                           IN ELRCON               
00098                           FILE STATUS     CHK.                    
00099                                                                   
00100      SELECT ELERRS        ASSIGN TO SYS010-FBA1-ELERRS            
00101                           ORGANIZATION    INDEXED                 
00102                           ACCESS          DYNAMIC                 
00103                           RECORD KEY      EM-CONTROL-PRIMARY      
00104                                           IN ELERRS               
00105                           FILE STATUS     CHK.                    
00106                                                                   
00107      SELECT ELFORM        ASSIGN TO SYS010-FBA1-ELFORM            
00108                           ORGANIZATION    INDEXED                 
00109                           ACCESS          DYNAMIC                 
00110                           RECORD KEY      TX-CONTROL-PRIMARY      
00111                                           IN ELFORM               
00112                           FILE STATUS     CHK.                    
00113                                                                   
00114      SELECT ELHELP        ASSIGN TO SYS010-FBA1-ELHELP            
00115                           ORGANIZATION    INDEXED                 
00116                           ACCESS          DYNAMIC                 
00117                           RECORD KEY      TX-CONTROL-PRIMARY      
00118                                           IN ELHELP               
00119                           FILE STATUS     CHK.                    
00120                                                                   
00121      SELECT ELLETR        ASSIGN TO SYS010-FBA1-ELLETR            
00122                           ORGANIZATION    INDEXED                 
00123                           ACCESS          DYNAMIC                 
00124                           RECORD KEY      TX-CONTROL-PRIMARY      
00125                                           IN ELLETR               
00126                           FILE STATUS     CHK.                    
00127                                                                   
00128      SELECT ELMSTR        ASSIGN TO SYS010-FBA1-ELMSTR            
00129                           ORGANIZATION    INDEXED                 
00130                           ACCESS          DYNAMIC                 
00131                           RECORD KEY      CL-CONTROL-PRIMARY      
00132                                           IN ELMSTR               
00133                           FILE STATUS     CHK.                    
00134                                                                   
00135      SELECT ELRETR        ASSIGN TO SYS010-FBA1-ELRETR            
00136                           ORGANIZATION    INDEXED                 
00137                           ACCESS          DYNAMIC                 
00138                           RECORD KEY      RL-CONTROL-PRIMARY      
00139                                           IN ELRETR               
00140                           FILE STATUS     CHK.                    
00141                                                                   
00142      SELECT ELPEND        ASSIGN TO SYS010-FBA1-ELPEND            
00143                           ORGANIZATION    INDEXED                 
00144                           ACCESS          DYNAMIC                 
00145                           RECORD KEY      SU-CONTROL-PRIMARY      
00146                                           IN ELPEND               
00147                           FILE STATUS     CHK.                    
00148                                                                   
00149      SELECT ELPGMN        ASSIGN TO SYS010-FBA1-ELPGMN            
00150                           ORGANIZATION    INDEXED                 
00151                           ACCESS          DYNAMIC                 
00152                           RECORD KEY      PN-CONTROL-PRIMARY      
00153                                           IN ELPGMN               
00154                           FILE STATUS     CHK.                    
00155                                                                   
00156      SELECT ELPGMO        ASSIGN TO SYS010-FBA1-ELPGMO            
00157                           ORGANIZATION    INDEXED                 
00158                           ACCESS          DYNAMIC                 
00159                           RECORD KEY      PO-CONTROL-PRIMARY      
00160                                           IN ELPGMO               
00161                           FILE STATUS     CHK.                    
00162                                                                   
00163      SELECT ELPGMS        ASSIGN TO SYS010-FBA1-ELPGMS            
00164                           ORGANIZATION    INDEXED                 
00165                           ACCESS          DYNAMIC                 
00166                           RECORD KEY      PS-CONTROL-PRIMARY      
00167                                           IN ELPGMS               
00168                           FILE STATUS     CHK.                    
00169                                                                   
00170      SELECT ELPOLF        ASSIGN TO SYS010-FBA1-ELPOLF            
00171                           ORGANIZATION    INDEXED                 
00172                           ACCESS          DYNAMIC                 
00173                           RECORD KEY      PF-CONTROL-PRIMARY      
00174                                           IN ELPOLF               
00175                           FILE STATUS     CHK.                    
00176                                                                   
00177      SELECT ELTRLR        ASSIGN TO SYS010-FBA1-ELTRLR            
00178                           ORGANIZATION    INDEXED                 
00179                           ACCESS          DYNAMIC                 
00180                           RECORD KEY      AT-CONTROL-PRIMARY      
00181                                           IN ELTRLR               
00182                           FILE STATUS     CHK.                    
00183                                                                   
00184      SELECT ELREPT        ASSIGN TO SYS010-FBA1-ELREPT            
00185                           ORGANIZATION    INDEXED                 
00186                           ACCESS          DYNAMIC                 
00187                           RECORD KEY      RF-CONTROL-PRIMARY      
00188                                           IN ELREPT               
00189                           FILE STATUS     CHK.                    
00190                                                                   
00191      SELECT ERACCT        ASSIGN TO SYS010-FBA1-ERACCT            
00192                           ORGANIZATION    INDEXED                 
00193                           ACCESS          DYNAMIC                 
00194                           RECORD KEY      AM-CONTROL-PRIMARY      
00195                                           IN ERACCT               
00196                           FILE STATUS     CHK.                    
00197                                                                   
00198      SELECT ERACNT        ASSIGN TO SYS010-FBA1-ERACNT            
00199                           ORGANIZATION    INDEXED                 
00200                           ACCESS          DYNAMIC                 
00201                           RECORD KEY      NT-CONTROL-PRIMARY      
00202                                           IN ERACNT               
00203                           FILE STATUS     CHK.                    
00204                                                                   
00205      SELECT ERARCH        ASSIGN TO SYS010-FBA1-ERARCH            
00206                           ORGANIZATION    INDEXED                 
00207                           ACCESS          DYNAMIC                 
00208                           RECORD KEY      LA-CONTROL-PRIMARY      
00209                                           IN ERARCH               
00210                           FILE STATUS     CHK.                    
00211                                                                   
00212      SELECT ERARCT        ASSIGN TO SYS010-FBA1-ERARCT            
00213                           ORGANIZATION    INDEXED                 
00214                           ACCESS          DYNAMIC                 
00215                           RECORD KEY      LT-CONTROL-PRIMARY      
00216                                           IN ERARCT               
00217                           FILE STATUS     CHK.                    
00218                                                                   
00219      SELECT ERBILL        ASSIGN TO SYS010-FBA1-ERBILL            
00220                           ORGANIZATION    INDEXED                 
00221                           ACCESS          DYNAMIC                 
00222                           RECORD KEY      BI-CONTROL-PRIMARY      
00223                                           IN ERBILL               
00224                           FILE STATUS     CHK.                    
00225                                                                   
00226      SELECT ERCHEK        ASSIGN TO SYS010-FBA1-ERCHEK            
00227                           ORGANIZATION    INDEXED                 
00228                           ACCESS          DYNAMIC                 
00229                           RECORD KEY      CH-CONTROL-PRIMARY      
00230                                           IN ERCHEK               
00231                           FILE STATUS     CHK.                    
00232                                                                   
00233      SELECT ERCHKQ        ASSIGN TO SYS010-FBA1-ERCHKQ            
00234                           ORGANIZATION    INDEXED                 
00235                           ACCESS          DYNAMIC                 
00236                           RECORD KEY      CQ-CONTROL-PRIMARY      
00237                                           IN ERCHKQ               
00238                           FILE STATUS     CHK.                    
00239                                                                   
00240      SELECT ERCOMP        ASSIGN TO SYS010-FBA1-ERCOMP            
00241                           ORGANIZATION    INDEXED                 
00242                           ACCESS          DYNAMIC                 
00243                           RECORD KEY      CO-CONTROL-PRIMARY      
00244                                           IN ERCOMP               
00245                           FILE STATUS     CHK.                    
00246                                                                   
00247      SELECT ERCRTC        ASSIGN TO SYS010-FBA1-ERCRTC            
00248                           ORGANIZATION    INDEXED                 
00249                           ACCESS          DYNAMIC                 
00250                           RECORD KEY      CC-CONTROL-PRIMARY      
00251                                           IN ERCRTC               
00252                           FILE STATUS     CHK.                    
00253                                                                   
00254      SELECT ERCTBL        ASSIGN TO SYS010-FBA1-ERCTBL            
00255                           ORGANIZATION    INDEXED                 
00256                           ACCESS          DYNAMIC                 
00257                           RECORD KEY      CT-CONTROL-PRIMARY      
00258                                           IN ERCTBL               
00259                           FILE STATUS     CHK.                    
00260                                                                   
00261      SELECT ERNAME        ASSIGN TO SYS010-FBA1-ERNAME            
00262                           ORGANIZATION    INDEXED                 
00263                           ACCESS          DYNAMIC                 
00264                           RECORD KEY      NL-RECORD-KEY           
00265                                           IN ERNAME               
00266                           FILE STATUS     CHK.                    
00267                                                                   
00268      SELECT ERPLAN        ASSIGN TO SYS010-FBA1-ERPLAN            
00269                           ORGANIZATION    INDEXED                 
00270                           ACCESS          DYNAMIC                 
00271                           RECORD KEY      PL-CONTROL-PRIMARY      
00272                                           IN ERPLAN               
00273                           FILE STATUS     CHK.                    
00274                                                                   
00275      SELECT ERPNDB        ASSIGN TO SYS010-FBA1-ERPNDB            
00276                           ORGANIZATION    INDEXED                 
00277                           ACCESS          DYNAMIC                 
00278                           RECORD KEY      PB-CONTROL-PRIMARY      
00279                                           IN ERPNDB               
00280                           FILE STATUS     CHK.                    
00281                                                                   
00282      SELECT ERPNDE        ASSIGN TO SYS010-FBA1-ERPNDE            
00283                           ORGANIZATION    INDEXED                 
00284                           ACCESS          DYNAMIC                 
00285                           RECORD KEY      PB-CONTROL-PRIMARY      
00286                                           IN ERPNDE               
00287                           FILE STATUS     CHK.                    
00288                                                                   
00289      SELECT ERPNDC        ASSIGN TO SYS010-FBA1-ERPNDC            
00290                           ORGANIZATION    INDEXED                 
00291                           ACCESS          DYNAMIC                 
00292                           RECORD KEY      PC-CONTROL-PRIMARY      
00293                                           IN ERPNDC               
00294                           FILE STATUS     CHK.                    
00295                                                                   
00296      SELECT ERPYAJ        ASSIGN TO SYS010-FBA1-ERPYAJ            
00297                           ORGANIZATION    INDEXED                 
00298                           ACCESS          DYNAMIC                 
00299                           RECORD KEY      PY-CONTROL-PRIMARY      
00300                                           IN ERPYAJ               
00301                           FILE STATUS     CHK.                    
00302                                                                   
00303      SELECT ERRATE        ASSIGN TO SYS010-FBA1-ERRATE            
00304                           ORGANIZATION    INDEXED                 
00305                           ACCESS          DYNAMIC                 
00306                           RECORD KEY      RT-CONTROL-PRIMARY      
00307                                           IN ERRATE               
00308                           FILE STATUS     CHK.                    
00309                                                                   
00310      SELECT ERREIN        ASSIGN TO SYS010-FBA1-ERREIN            
00311                           ORGANIZATION    INDEXED                 
00312                           ACCESS          DYNAMIC                 
00313                           RECORD KEY      RE-CONTROL-PRIMARY      
00314                                           IN ERREIN               
00315                           FILE STATUS     CHK.                    
00316                                                                   
00317      SELECT ERREPY        ASSIGN TO SYS010-FBA1-ERREPY            
00318                           ORGANIZATION    INDEXED                 
00319                           ACCESS          DYNAMIC                 
00320                           RECORD KEY      RP-CONTROL-PRIMARY      
00321                                           IN ERREPY               
00322                           FILE STATUS     CHK.                    
00323                                                                   
00324      SELECT ERNOTE        ASSIGN TO SYS010-FBA1-ERNOTE            
00325                           ORGANIZATION    INDEXED                 
00326                           ACCESS          DYNAMIC                 
00327                           RECORD KEY      CN-CONTROL-PRIMARY      
00328                                           IN ERNOTE               
00329                           FILE STATUS     CHK.                    
00330                                                                   
00331      SELECT ERGXRF        ASSIGN TO SYS010-FBA1-ERGXRF            
00332                           ORGANIZATION    INDEXED                 
00333                           ACCESS          DYNAMIC                 
00334                           RECORD KEY      GX-CONTROL-PRIMARY      
00335                                           IN ERGXRF               
00336                           FILE STATUS     CHK.                    
00337                                                                   
00338      SELECT ERCOMM        ASSIGN TO SYS010-FBA1-ERCOMM            
00339                           ORGANIZATION    INDEXED                 
00340                           ACCESS          DYNAMIC                 
00341                           RECORD KEY      CE-CONTROL-PRIMARY      
00342                                           IN ERCOMM               
00343                           FILE STATUS     CHK.                    
00344                                                                   
00345      SELECT ERLOFC        ASSIGN TO SYS010-FBA1-ERLOFC            
00346                           ORGANIZATION    INDEXED                 
00347                           ACCESS          DYNAMIC                 
00348                           RECORD KEY      LO-CONTROL-PRIMARY      
00349                                           IN ERLOFC               
00350                           FILE STATUS     CHK.                    
00351                                                                   
00352      SELECT ERMEBL        ASSIGN TO SYS010-FBA1-ERMEBL            
00353                           ORGANIZATION    INDEXED                 
00354                           ACCESS          DYNAMIC                 
00355                           RECORD KEY      ME-CONTROL-PRIMARY      
00356                                           IN ERMEBL               
00357                           FILE STATUS     CHK.                    
00358                                                                   
00359      SELECT ERMAIL        ASSIGN TO SYS010-FBA1-ERMAIL            
00360                           ORGANIZATION    INDEXED                 
00361                           ACCESS          DYNAMIC                 
00362                           RECORD KEY      MA-CONTROL-PRIMARY      
00363                                           IN ERMAIL               
00364                           FILE STATUS     CHK.                    
00365                                                                   
00366      SELECT ERPNDM        ASSIGN TO SYS010-FBA1-ERPNDM            
00367                           ORGANIZATION    INDEXED                 
00368                           ACCESS          DYNAMIC                 
00369                           RECORD KEY      PM-CONTROL-PRIMARY      
00370                                           IN ERPNDM               
00371                           FILE STATUS     CHK.                    
00372                                                                   
00373      SELECT ERRQST        ASSIGN TO SYS010-FBA1-ERRQST            
00374                           ORGANIZATION    INDEXED                 
00375                           ACCESS          DYNAMIC                 
00376                           RECORD KEY      RQ-CONTROL-PRIMARY      
00377                                           IN ERRQST               
00378                           FILE STATUS     CHK.                    
00379                                                                   
00380      SELECT ERSUMM        ASSIGN TO SYS010-FBA1-ERSUMM            
00381                           ORGANIZATION    INDEXED                 
00382                           ACCESS          DYNAMIC                 
00383                           RECORD KEY      SX-CONTROL-PRIMARY      
00384                                           IN ERSUMM               
00385                           FILE STATUS     CHK.                    
00386                                                                   
00387      SELECT ERRECV        ASSIGN TO SYS010-FBA1-ERRECV            
00388                           ORGANIZATION    INDEXED                 
00389                           ACCESS          DYNAMIC                 
00390                           RECORD KEY      AR-CONTROL-PRIMARY      
00391                                           IN ERRECV               
00392                           FILE STATUS     CHK.                    
00393                                                                   
00394      SELECT ERCKWK        ASSIGN TO SYS010-FBA1-ERCKWK            
00395                           ORGANIZATION    INDEXED                 
00396                           ACCESS          DYNAMIC                 
00397                           RECORD KEY      CW-CONTROL-PRIMARY      
00398                                           IN ERCKWK               
00399                           FILE STATUS     CHK.                    
00400                                                                   
00401      SELECT ERCMCK        ASSIGN TO SYS010-FBA1-ERCMCK            
00402                           ORGANIZATION    INDEXED                 
00403                           ACCESS          DYNAMIC                 
00404                           RECORD KEY      CK-CONTROL-PRIMARY      
00405                                           IN ERCMCK               
00406                           FILE STATUS     CHK.                    
00407                                                                   
00408      SELECT ERCMKQ        ASSIGN TO SYS010-FBA1-ERCMKQ            
00409                           ORGANIZATION    INDEXED                 
00410                           ACCESS          DYNAMIC                 
00411                           RECORD KEY      MQ-CONTROL-PRIMARY      
00412                                           IN ERCMKQ               
00413                           FILE STATUS     CHK.                    
00414                                                                   
00415      SELECT ERFORM        ASSIGN TO SYS010-FBA1-ERFORM            
00416                           ORGANIZATION    INDEXED                 
00417                           ACCESS          DYNAMIC                 
00418                           RECORD KEY      FO-CONTROL-PRIMARY      
00419                                           IN ERFORM               
00420                           FILE STATUS     CHK.                    
00421      SELECT ERRTRO        ASSIGN TO SYS010-FBA1-ERRTRO            
00422                           ORGANIZATION    INDEXED                 
00423                           ACCESS          DYNAMIC                 
00424                           RECORD KEY      RM-CONTROL-PRIMARY      
00425                                           IN ERRTRO               
00426                           FILE STATUS     CHK.                    
00427                                                                   
00428      SELECT ERINMS        ASSIGN TO SYS010-FBA1-ERINMS            
00429                           ORGANIZATION    INDEXED                 
00430                           ACCESS          DYNAMIC                 
00431                           RECORD KEY      IM-CONTROL-PRIMARY      
00432                           FILE STATUS     CHK.                    
00433                                                                   
00434      SELECT ERARBR        ASSIGN TO SYS010-FBA1-ERARBR            
00435                           ORGANIZATION    INDEXED                 
00436                           ACCESS          DYNAMIC                 
00437                           RECORD KEY      AB-CONTROL-PRIMARY      
00438                           FILE STATUS     CHK.                    
00439                                                                   
00440      SELECT ERCCAP        ASSIGN TO SYS010-FBA1-ERCCAP            
00441                           ORGANIZATION    INDEXED                 
00442                           ACCESS          DYNAMIC                 
00443                           RECORD KEY      ERCCAP-PRIMARY-KEY      
00444                           FILE STATUS     CHK.                    
00445                                                                   
00446      SELECT ELNOTE        ASSIGN TO SYS010-FBA1-ELNOTE            
00447                           ORGANIZATION    INDEXED                 
00448                           ACCESS          DYNAMIC                 
00449                           RECORD KEY      EN-CONTROL-PRIMARY      
00450                           FILE STATUS     CHK.                    
00451                                                                   
00452      SELECT ERRESS        ASSIGN TO SYS010-FBA1-ERRESS            
00453                           ORGANIZATION    INDEXED                 
00454                           ACCESS          DYNAMIC                 
00455                           RECORD KEY      ERRESS-PRIMARY-KEY      
00456                           FILE STATUS     CHK.                    
00457                                                                   
00458      SELECT ERRESC        ASSIGN TO SYS010-FBA1-ERRESC            
00459                           ORGANIZATION    INDEXED                 
00460                           ACCESS          DYNAMIC                 
00461                           RECORD KEY      ERRESC-RECORD-KEY       
00462                           FILE STATUS     CHK.                    
00463                                                                   
00464      SELECT ELACHP        ASSIGN TO SYS010-FBA1-ELACHP            
00465                           ORGANIZATION    INDEXED                 
00466                           ACCESS          DYNAMIC                 
00467                           RECORD KEY      AP-CONTROL-PRIMARY      
00468                                           IN ELACHP               
00469                           FILE STATUS     CHK.                    
00470                                                                   
00471      SELECT ELBANK        ASSIGN TO SYS010-FBA1-ELBANK            
00472                           ORGANIZATION    INDEXED                 
00473                           ACCESS          DYNAMIC                 
00474                           RECORD KEY      BM-CONTROL-PRIMARY      
00475                                           IN ELBANK               
00476                           FILE STATUS     CHK.                    
00477      SELECT ERLOSS        ASSIGN TO SYS010-FBA1-ERLOSS            
00478                           ORGANIZATION    INDEXED                 
00479                           ACCESS          DYNAMIC                 
00480                           RECORD KEY      LR-CONTROL              
00481                                           IN ERLOSS               
00482                           FILE STATUS     CHK.                    
00483      EJECT                                                        
00484                                                                   
00485  DATA DIVISION.                                                   
00486                                                                   
00487  FILE SECTION.                                                    
00488                                                                   
00489  FD  CARD-FILE                                                    
00490      BLOCK CONTAINS 0 RECORDS
00491      RECORDING MODE F.                                            
00492                                                                   
00493  01  CARD-RECORD.                                                 
00494      05  ER-FILE-NAME                PIC X(8).                    
00495      05  ER-ACTION                   PIC X(6).                    
00496      05  ER-COMPANY                  PIC 99.                      
00497      05  FILLER                      PIC X(64).                   
00498                                                                   
00499  EJECT                                                            
00500  FD  ELACTQ.                                                      
00501                                                                   
00502      COPY ELCACTQ.                                                
00503                                                                   
00504  EJECT                                                            
00505  FD  ELALPH.                                                      
00506                                                                   
00507      COPY ELCALPH.                                                
00508                                                                   
00509  EJECT                                                            
00510  FD  ELARCH.                                                      
00511                                                                   
00512      COPY ELCARCH.                                                
00513                                                                   
00514  EJECT                                                            
00515  FD  ELARCT.                                                      
00516                                                                   
00517      COPY ELCARCT.                                                
00518                                                                   
00519  EJECT                                                            
00520  FD  ELBENE.                                                      
00521                                                                   
00522      COPY ELCBENE.                                                
00523                                                                   
00524  EJECT                                                            
00525  FD  ELCERT.                                                      
00526                                                                   
00527      COPY ELCCERT.                                                
00528                                                                   
00529  EJECT                                                            
00525  FD  ELPURG.                                                      
00526                                                                   
00527      COPY ELCPURG.                                                
00528                                                                   
00529  EJECT                                                            
00530  FD  ELCHKQ.                                                      
00531                                                                   
00532      COPY ELCCHKQ.                                                
00533                                                                   
00534  EJECT                                                            
00535  FD  ELCNTL.                                                      
00536                                                                   
00537      COPY ELCCNTL.                                                
00538                                                                   
00539  EJECT                                                            
00540  FD  ELRCON.                                                      
00541                                                                   
00542      COPY ELCRCON.                                                
00543                                                                   
00544  EJECT                                                            
00545  FD  ELERRS.                                                      
00546                                                                   
00547      COPY ELCERRS.                                                
00548                                                                   
00549  EJECT                                                            
00550  FD  ELFORM.                                                      
00551                                                                   
00552      COPY ELCTEXT.                                                
00553                                                                   
00554  EJECT                                                            
00555  FD  ELHELP.                                                      
00556                                                                   
00557      COPY ELCTEXT.                                                
00558                                                                   
00559  EJECT                                                            
00560  FD  ELLETR.                                                      
00561                                                                   
00562      COPY ELCTEXT.                                                
00563                                                                   
00564  EJECT                                                            
00565  FD  ELMSTR.                                                      
00566                                                                   
00567      COPY ELCMSTR.                                                
00568                                                                   
00569  EJECT                                                            
00570  FD  ELRETR.                                                      
00571                                                                   
00572      COPY ELCRETR.                                                
00573                                                                   
00574  EJECT                                                            
00575  FD  ELPGMN.                                                      
00576                                                                   
00577      COPY ELCPGMN.                                                
00578                                                                   
00579  EJECT                                                            
00580  FD  ELPGMO.                                                      
00581                                                                   
00582      COPY ELCPGMO.                                                
00583                                                                   
00584  EJECT                                                            
00585  FD  ELPGMS.                                                      
00586                                                                   
00587      COPY ELCPGMS.                                                
00588                                                                   
00589  EJECT                                                            
00590  FD  ELTRLR.                                                      
00591                                                                   
00592      COPY ELCTRLR.                                                
00593                                                                   
00594  EJECT                                                            
00595  FD  ELREPT.                                                      
00596                                                                   
00597      COPY ELCREPT.                                                
00598                                                                   
00599  EJECT                                                            
00600  FD  ERACCT.                                                      
00601                                                                   
00602      COPY ERCACCT.                                                
00603                                                                   
00604  FD  ERACNT.                                                      
00605                                                                   
00606      COPY ERCACNT.                                                
00607                                                                   
00608  EJECT                                                            
00609  FD  ERARCH.                                                      
00610                                                                   
00611      COPY ERCARCH.                                                
00612                                                                   
00613  EJECT                                                            
00614  FD  ERARCT.                                                      
00615                                                                   
00616      COPY ERCARCT.                                                
00617                                                                   
00618  EJECT                                                            
00619  FD  ERBILL.                                                      
00620                                                                   
00621      COPY ERCBILL.                                                
00622                                                                   
00623  EJECT                                                            
00624  FD  ERCHKQ.                                                      
00625                                                                   
00626      COPY ERCCHKQ.                                                
00627                                                                   
00628  EJECT                                                            
00629  FD  ERCHEK.                                                      
00630                                                                   
00631      COPY ERCCHEK.                                                
00632                                                                   
00633  EJECT                                                            
00634  FD  ERCOMP.                                                      
00635                                                                   
00636      COPY ERCCOMP.                                                
00637                                                                   
00638  EJECT                                                            
00639  FD  ERCRTC.                                                      
00640                                                                   
00641      COPY ERCCRTC.                                                
00642                                                                   
00643  EJECT                                                            
00644  FD  ERCTBL.                                                      
00645                                                                   
00646      COPY ERCCTBL.                                                
00647                                                                   
00648  EJECT                                                            
00649  FD  ERNAME.                                                      
00650                                                                   
00651      COPY ERCNAME.                                                
00652                                                                   
00653  EJECT                                                            
00654  FD  ERPLAN.                                                      
00655                                                                   
00656      COPY ERCPLAN.                                                
00657                                                                   
00658  EJECT                                                            
00659  FD  ERPNDB.                                                      
00660                                                                   
00661      COPY ERCPNDB.                                                
00662                                                                   
00663  EJECT                                                            
00664  FD  ERPNDE.                                                      
00665                                                                   
00666      COPY ERCPNDB.                                                
00667                                                                   
00668  EJECT                                                            
00669  FD  ERPNDC.                                                      
00670                                                                   
00671      COPY ERCPNDC.                                                
00672                                                                   
00673  EJECT                                                            
00674  FD  ERRATE.                                                      
00675                                                                   
00676      COPY ERCRATE.                                                
00677                                                                   
00678  EJECT                                                            
00679  FD  ERREIN.                                                      
00680                                                                   
00681      COPY ERCREIN.                                                
00682                                                                   
00683  EJECT                                                            
00684  FD  ERREPY.                                                      
00685                                                                   
00686      COPY ERCREPY.                                                
00687                                                                   
00688  EJECT                                                            
00689  FD  ERPYAJ.                                                      
00690                                                                   
00691      COPY ERCPYAJ.                                                
00692                                                                   
00693  EJECT                                                            
00694  FD  ERNOTE.                                                      
00695                                                                   
00696      COPY ERCNOTE.                                                
00697                                                                   
00698  EJECT                                                            
00699  FD  ERGXRF.                                                      
00700                                                                   
00701      COPY ERCGXRF.                                                
00702                                                                   
00703  EJECT                                                            
00704  FD  ERCOMM.                                                      
00705                                                                   
00706      COPY ERCCOMM.                                                
00707                                                                   
00708  EJECT                                                            
00709  FD  ERLOFC.                                                      
00710                                                                   
00711      COPY ERCLOFC.                                                
00712                                                                   
00713  EJECT                                                            
00714  FD  ERMEBL.                                                      
00715                                                                   
00716      COPY ERCMEBL.                                                
00717                                                                   
00718  EJECT                                                            
00719  FD  ERMAIL.                                                      
00720                                                                   
00721      COPY ERCMAIL.                                                
00722                                                                   
00723  EJECT                                                            
00724  FD  ERPNDM.                                                      
00725                                                                   
00726      COPY ERCPNDM.                                                
00727                                                                   
00728  EJECT                                                            
00729  FD  ERRQST.                                                      
00730                                                                   
00731      COPY ERCRQST.                                                
00732                                                                   
00733  EJECT                                                            
00734  FD  ERSUMM.                                                      
00735                                                                   
00736      COPY ERCSUMM.                                                
00737                                                                   
00738  EJECT                                                            
00739  FD  ERRECV.                                                      
00740                                                                   
00741      COPY ERCRECV.                                                
00742                                                                   
00743  EJECT                                                            
00744  FD  ERCKWK.                                                      
00745                                                                   
00746      COPY ERCCKWK.                                                
00747                                                                   
00748  EJECT                                                            
00749  FD  ERCMCK.                                                      
00750                                                                   
00751      COPY ERCCMCK.                                                
00752                                                                   
00753  EJECT                                                            
00754  FD  ERCMKQ.                                                      
00755                                                                   
00756      COPY ERCCMKQ.                                                
00757                                                                   
00758  EJECT                                                            
00759  FD  ERFORM.                                                      
00760                                                                   
00761      COPY ERCFORM.                                                
00762                                                                   
00763  FD  ERRTRO.                                                      
00764                                                                   
00765      COPY ERCRTRO.                                                
00766                                                                   
00767      EJECT                                                        
00768  FD  ELPEND.                                                      
00769                                                                   
00770      COPY ELCPEND.                                                
00771      EJECT                                                        
00772  FD  ELPOLF.                                                      
00773                                                                   
00774      COPY ELCPOLF.                                                
00775                                                                   
00776      EJECT                                                        
00777  FD  ERINMS.                                                      
00778                                                                   
00779      COPY INVMSTR.                                                
00780                                                                   
00781      EJECT                                                        
00782  FD  ERARBR.                                                      
00783                                                                   
00784      COPY ERCARBR.                                                
00785                                                                   
00786      EJECT                                                        
00787  FD  ERCCAP.                                                      
00788                                                                   
00789      COPY ERCCAPS.                                                
00790                                                                   
00791      EJECT                                                        
00792  FD  ELNOTE.                                                      
00793                                                                   
00794      COPY ELCNOTE.                                                
00795                                                                   
00796      EJECT                                                        
00797  FD  ERRESS.                                                      
00798                                                                   
00799      COPY ERCRESS.                                                
00800                                                                   
00801      EJECT                                                        
00802  FD  ERRESC.                                                      
00803                                                                   
00804      COPY ERCRESC.                                                
00805      EJECT                                                        
00806  FD  ELACHP.                                                      
00807                                                                   
00808      COPY ELCACHP.                                                
00809      EJECT                                                        
00810  FD  ELBANK.                                                      
00811                                                                   
00812      COPY ELCBANK.                                                
00813      EJECT                                                        
00814  FD  ERLOSS.                                                      
00815                                                                   
00816      COPY ERCLOSS.                                                
00817      EJECT                                                        
00818  WORKING-STORAGE SECTION.                                         
00819                                                                   
00820  77  FILLER  PIC X(32)  VALUE '********************************'. 
00821  77  FILLER  PIC X(32)  VALUE '*   EL349  WORKING STORAGE     *'. 
00822  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.022 ***********'. 
00823                                                                   
00824  01  FILLER.                                                      
00825      05  CHK.                                                     
00826          10  CHK1               PIC X.                            
00827          10  CHK2               PIC X.                            
00828                                                                   
00829      EJECT                                                        
00830  PROCEDURE DIVISION.                                              
00831                                                                   
00832      OPEN INPUT CARD-FILE.                                        
00833                                                                   
00834  0100-READ.                                                       
00835      READ CARD-FILE                                               
00836          AT END  GO TO 9999-EOJ.                                  
00837                                                                   
00838      DISPLAY ' '.                                                 
00839      DISPLAY CARD-RECORD.                                         
00840  EJECT                                                            
00841      IF ER-FILE-NAME = 'ELACTQ'                                   
00842        AND ER-ACTION = 'LOAD'                                     
00843          OPEN OUTPUT ELACTQ                                       
00844          PERFORM 1000-ERROR-CHECK                                 
00845          MOVE LOW-VALUES   TO ACTIVITY-QUE       IN ELACTQ        
00846          MOVE ALL '9'      TO AQ-CONTROL-PRIMARY IN ELACTQ        
00847          WRITE ACTIVITY-QUE IN ELACTQ                             
00848          PERFORM 1000-ERROR-CHECK                                 
00849          CLOSE ELACTQ                                             
00850          PERFORM 1000-ERROR-CHECK                                 
00851          DISPLAY 'ELACTQ IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00852                                                                   
00853      IF ER-FILE-NAME = 'ELACTQ'                                   
00854        AND ER-ACTION = 'DELETE'                                   
00855          OPEN I-O ELACTQ                                          
00856          PERFORM 1000-ERROR-CHECK                                 
00857          MOVE ALL '9'      TO AQ-CONTROL-PRIMARY IN ELACTQ        
00858          DELETE ELACTQ                                            
00859          PERFORM 1000-ERROR-CHECK                                 
00860          CLOSE ELACTQ                                             
00861          PERFORM 1000-ERROR-CHECK                                 
00862          DISPLAY 'ELACTQ IS UPDATED (UNLESS NOTED ABOVE)'.        
00863                                                                   
00864      IF ER-FILE-NAME = 'ELALPH'                                   
00865        AND ER-ACTION = 'LOAD'                                     
00866          OPEN OUTPUT ELALPH                                       
00867          PERFORM 1000-ERROR-CHECK                                 
00868          MOVE LOW-VALUES   TO ALPHA-INDEX        IN ELALPH        
00869          MOVE ALL '9'      TO AI-CONTROL-PRIMARY IN ELALPH        
00870          WRITE ALPHA-INDEX  IN ELALPH                             
00871          PERFORM 1000-ERROR-CHECK                                 
00872          CLOSE ELALPH                                             
00873          PERFORM 1000-ERROR-CHECK                                 
00874          DISPLAY 'ELALPH IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00875                                                                   
00876      IF ER-FILE-NAME = 'ELALPH'                                   
00877        AND ER-ACTION = 'DELETE'                                   
00878          OPEN I-O ELALPH                                          
00879          PERFORM 1000-ERROR-CHECK                                 
00880          MOVE ALL '9'      TO AI-CONTROL-PRIMARY IN ELALPH        
00881          DELETE ELALPH                                            
00882          PERFORM 1000-ERROR-CHECK                                 
00883          CLOSE ELALPH                                             
00884          PERFORM 1000-ERROR-CHECK                                 
00885          DISPLAY 'ELALPH IS UPDATED (UNLESS NOTED ABOVE)'.        
00886                                                                   
00887      IF ER-FILE-NAME = 'ELARCH'                                   
00888        AND ER-ACTION = 'LOAD'                                     
00889          OPEN OUTPUT ELARCH                                       
00890          PERFORM 1000-ERROR-CHECK                                 
00891          MOVE LOW-VALUES        TO LETTER-ARCHIVE     IN ELARCH   
00892          MOVE ALL '9'           TO LA-CONTROL-PRIMARY IN ELARCH   
00893          WRITE LETTER-ARCHIVE IN ELARCH                           
00894          PERFORM 1000-ERROR-CHECK                                 
00895          CLOSE ELARCH                                             
00896          PERFORM 1000-ERROR-CHECK                                 
00897          DISPLAY 'ELARCH IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00898                                                                   
00899      IF ER-FILE-NAME = 'ELARCH'                                   
00900        AND ER-ACTION = 'DELETE'                                   
00901          OPEN I-O ELARCH                                          
00902          PERFORM 1000-ERROR-CHECK                                 
00903          MOVE ALL '9'        TO LA-CONTROL-PRIMARY IN ELARCH      
00904          DELETE ELARCH                                            
00905          PERFORM 1000-ERROR-CHECK                                 
00906          CLOSE ELARCH                                             
00907          PERFORM 1000-ERROR-CHECK                                 
00908          DISPLAY 'ELARCH IS UPDATED (UNLESS NOTED ABOVE)'.        
00909  EJECT                                                            
00910      IF ER-FILE-NAME = 'ELARCT'                                   
00911        AND ER-ACTION = 'LOAD'                                     
00912          OPEN OUTPUT ELARCT                                       
00913          PERFORM 1000-ERROR-CHECK                                 
00914          MOVE LOW-VALUES        TO LETTER-ARCHIVE-TEMP IN ELARCT  
00915          MOVE ALL '9'           TO LT-CONTROL-PRIMARY  IN ELARCT  
00916          WRITE LETTER-ARCHIVE-TEMP IN ELARCT                      
00917          PERFORM 1000-ERROR-CHECK                                 
00918          CLOSE ELARCT                                             
00919          PERFORM 1000-ERROR-CHECK                                 
00920          DISPLAY 'ELARCT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00921                                                                   
00922      IF ER-FILE-NAME = 'ELARCT'                                   
00923        AND ER-ACTION = 'DELETE'                                   
00924          OPEN I-O ELARCT                                          
00925          PERFORM 1000-ERROR-CHECK                                 
00926          MOVE ALL '9'        TO LT-CONTROL-PRIMARY IN ELARCT      
00927          DELETE ELARCT                                            
00928          PERFORM 1000-ERROR-CHECK                                 
00929          CLOSE ELARCT                                             
00930          PERFORM 1000-ERROR-CHECK                                 
00931          DISPLAY 'ELARCT IS UPDATED (UNLESS NOTED ABOVE)'.        
00932  EJECT                                                            
00933      IF ER-FILE-NAME = 'ELBENE'                                   
00934        AND ER-ACTION = 'LOAD'                                     
00935          OPEN OUTPUT ELBENE                                       
00936          PERFORM 1000-ERROR-CHECK                                 
00937          MOVE LOW-VALUES      TO BENEFICIARY-MASTER IN ELBENE     
00938          MOVE ALL '9'         TO BE-CONTROL-PRIMARY IN ELBENE     
00939          WRITE BENEFICIARY-MASTER IN ELBENE                       
00940          PERFORM 1000-ERROR-CHECK                                 
00941          CLOSE ELBENE                                             
00942          PERFORM 1000-ERROR-CHECK                                 
00943          DISPLAY 'ELBENE IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00944                                                                   
00945      IF ER-FILE-NAME = 'ELBENE'                                   
00946        AND ER-ACTION = 'DELETE'                                   
00947          OPEN I-O ELBENE                                          
00948          PERFORM 1000-ERROR-CHECK                                 
00949          MOVE ALL '9'         TO BE-CONTROL-PRIMARY IN ELBENE     
00950          DELETE ELBENE                                            
00951          PERFORM 1000-ERROR-CHECK                                 
00952          CLOSE ELBENE                                             
00953          PERFORM 1000-ERROR-CHECK                                 
00954          DISPLAY 'ELBENE IS UPDATED (UNLESS NOTED ABOVE)'.        
00955                                                                   
00956      IF ER-FILE-NAME = 'ELCERT'                                   
00957        AND ER-ACTION = 'LOAD'                                     
00958          OPEN OUTPUT ELCERT                                       
00959          PERFORM 1000-ERROR-CHECK                                 
00960          MOVE LOW-VALUES   TO CERTIFICATE-MASTER IN ELCERT        
00961          MOVE ALL '9'      TO CM-CONTROL-PRIMARY IN ELCERT        
00962          WRITE CERTIFICATE-MASTER IN ELCERT                       
00963          PERFORM 1000-ERROR-CHECK                                 
00964          CLOSE ELCERT                                             
00965          PERFORM 1000-ERROR-CHECK                                 
00966          DISPLAY 'ELCERT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00967                                                                   
00968      IF ER-FILE-NAME = 'ELCERT'                                   
00969        AND ER-ACTION = 'DELETE'                                   
00970          OPEN I-O ELCERT                                          
00971          PERFORM 1000-ERROR-CHECK                                 
00972          MOVE ALL '9'      TO CM-CONTROL-PRIMARY IN ELCERT        
00973          DELETE ELCERT                                            
00974          PERFORM 1000-ERROR-CHECK                                 
00975          CLOSE ELCERT                                             
00976          PERFORM 1000-ERROR-CHECK                                 
00977          DISPLAY 'ELCERT IS UPDATED (UNLESS NOTED ABOVE)'.        
00978  EJECT                                                            
00956      IF ER-FILE-NAME = 'ELPURG'                                   
00957        AND ER-ACTION = 'LOAD'                                     
00958          OPEN OUTPUT ELPURG                                       
00959          PERFORM 1000-ERROR-CHECK                                 
00960          MOVE LOW-VALUES   TO PURGE-CERT-MASTER IN ELPURG         
00961          MOVE ALL '9'      TO PG-CONTROL-PRIMARY IN ELPURG        
00962          WRITE PURGE-CERT-MASTER IN ELPURG                        
00963          PERFORM 1000-ERROR-CHECK                                 
00964          CLOSE ELPURG                                             
00965          PERFORM 1000-ERROR-CHECK                                 
00966          DISPLAY 'ELPURG IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00967                                                                   
00968      IF ER-FILE-NAME = 'ELPURG'                                   
00969        AND ER-ACTION = 'DELETE'                                   
00970          OPEN I-O ELPURG                                          
00971          PERFORM 1000-ERROR-CHECK                                 
00972          MOVE ALL '9'      TO PG-CONTROL-PRIMARY IN ELPURG        
00973          DELETE ELPURG                                            
00974          PERFORM 1000-ERROR-CHECK                                 
00975          CLOSE ELPURG                                             
00976          PERFORM 1000-ERROR-CHECK                                 
00977          DISPLAY 'ELPURG IS UPDATED (UNLESS NOTED ABOVE)'.        
00978  EJECT                                                            
00979      IF ER-FILE-NAME = 'ELCHKQ'                                   
00980        AND ER-ACTION = 'LOAD'                                     
00981          OPEN OUTPUT ELCHKQ                                       
00982          PERFORM 1000-ERROR-CHECK                                 
00983          MOVE LOW-VALUES      TO CHECK-QUE          IN ELCHKQ     
00984          MOVE ALL '9'         TO CQ-CONTROL-PRIMARY IN ELCHKQ     
00985          MOVE ALL '9'         TO CQ-CONTROL-BY-PAYEE IN ELCHKQ    
00986          WRITE CHECK-QUE IN ELCHKQ                                
00987          PERFORM 1000-ERROR-CHECK                                 
00988          CLOSE ELCHKQ                                             
00989          PERFORM 1000-ERROR-CHECK                                 
00990          DISPLAY 'ELCHKQ IS INITIALIZED (UNLESS NOTED ABOVE)'.    
00991                                                                   
00992      IF ER-FILE-NAME = 'ELCHKQ'                                   
00993        AND ER-ACTION = 'DELETE'                                   
00994          OPEN I-O ELCHKQ                                          
00995          PERFORM 1000-ERROR-CHECK                                 
00996          MOVE ALL '9'       TO CQ-CONTROL-PRIMARY IN ELCHKQ       
00997          DELETE ELCHKQ                                            
00998          PERFORM 1000-ERROR-CHECK                                 
00999          CLOSE ELCHKQ                                             
01000          PERFORM 1000-ERROR-CHECK                                 
01001          DISPLAY 'ELCHKQ IS UPDATED (UNLESS NOTED ABOVE)'.        
01002                                                                   
01003      IF ER-FILE-NAME = 'ELCNTL'                                   
01004        AND ER-ACTION = 'LOAD'                                     
01005          OPEN OUTPUT ELCNTL                                       
01006          PERFORM 1000-ERROR-CHECK                                 
01007          MOVE LOW-VALUES     TO CONTROL-FILE       IN ELCNTL      
01008          MOVE ALL '9'        TO CF-CONTROL-PRIMARY IN ELCNTL      
01009          WRITE CONTROL-FILE IN ELCNTL                             
01010          PERFORM 1000-ERROR-CHECK                                 
01011          CLOSE ELCNTL                                             
01012          PERFORM 1000-ERROR-CHECK                                 
01013          DISPLAY 'ELCNTL IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01014                                                                   
01015      IF ER-FILE-NAME = 'ELCNTL'                                   
01016        AND ER-ACTION = 'DELETE'                                   
01017          OPEN I-O ELCNTL                                          
01018          PERFORM 1000-ERROR-CHECK                                 
01019          MOVE ALL '9'        TO CF-CONTROL-PRIMARY IN ELCNTL      
01020          DELETE ELCNTL                                            
01021          PERFORM 1000-ERROR-CHECK                                 
01022          CLOSE ELCNTL                                             
01023          PERFORM 1000-ERROR-CHECK                                 
01024          DISPLAY 'ELCNTL IS UPDATED (UNLESS NOTED ABOVE)'.        
01025  EJECT                                                            
01026      IF ER-FILE-NAME = 'ELRCON'                                   
01027        AND ER-ACTION = 'LOAD'                                     
01028          OPEN OUTPUT ELRCON                                       
01029          PERFORM 1000-ERROR-CHECK                                 
01030          MOVE LOW-VALUES  TO CHECK-RECONCILIATION  IN ELRCON      
01031          MOVE ALL '9'        TO RC-CONTROL-PRIMARY IN ELRCON      
01032          WRITE CHECK-RECONCILIATION IN ELRCON                     
01033          PERFORM 1000-ERROR-CHECK                                 
01034          CLOSE ELRCON                                             
01035          PERFORM 1000-ERROR-CHECK                                 
01036          DISPLAY 'ELRCON IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01037                                                                   
01038      IF ER-FILE-NAME = 'ELRCON'                                   
01039        AND ER-ACTION = 'DELETE'                                   
01040          OPEN I-O ELRCON                                          
01041          PERFORM 1000-ERROR-CHECK                                 
01042          MOVE ALL '9'        TO RC-CONTROL-PRIMARY IN ELRCON      
01043          DELETE ELRCON                                            
01044          PERFORM 1000-ERROR-CHECK                                 
01045          CLOSE ELRCON                                             
01046          PERFORM 1000-ERROR-CHECK                                 
01047          DISPLAY 'ELRCON IS UPDATED (UNLESS NOTED ABOVE)'.        
01048  EJECT                                                            
01049      IF ER-FILE-NAME = 'ELERRS'                                   
01050        AND ER-ACTION = 'LOAD'                                     
01051          OPEN OUTPUT ELERRS                                       
01052          PERFORM 1000-ERROR-CHECK                                 
01053          MOVE LOW-VALUES     TO ERROR-MESSAGE-FILE IN ELERRS      
01054          MOVE ALL '9'        TO EM-CONTROL-PRIMARY IN ELERRS      
01055          WRITE ERROR-MESSAGE-FILE IN ELERRS                       
01056          PERFORM 1000-ERROR-CHECK                                 
01057          CLOSE ELERRS                                             
01058          PERFORM 1000-ERROR-CHECK                                 
01059          DISPLAY 'ELERRS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01060                                                                   
01061      IF ER-FILE-NAME = 'ELERRS'                                   
01062        AND ER-ACTION = 'DELETE'                                   
01063          OPEN I-O ELERRS                                          
01064          PERFORM 1000-ERROR-CHECK                                 
01065          MOVE ALL '9'       TO EM-CONTROL-PRIMARY IN ELERRS       
01066          DELETE ELERRS                                            
01067          PERFORM 1000-ERROR-CHECK                                 
01068          CLOSE ELERRS                                             
01069          PERFORM 1000-ERROR-CHECK                                 
01070          DISPLAY 'ELERRS IS UPDATED (UNLESS NOTED ABOVE)'.        
01071  EJECT                                                            
01072      IF ER-FILE-NAME = 'ELFORM'                                   
01073        AND ER-ACTION = 'LOAD'                                     
01074          OPEN OUTPUT ELFORM                                       
01075          PERFORM 1000-ERROR-CHECK                                 
01076          MOVE LOW-VALUES     TO TEXT-FILES         IN ELFORM      
01077          MOVE ALL '9'        TO TX-CONTROL-PRIMARY IN ELFORM      
01078          WRITE TEXT-FILES IN ELFORM                               
01079          PERFORM 1000-ERROR-CHECK                                 
01080          CLOSE ELFORM                                             
01081          PERFORM 1000-ERROR-CHECK                                 
01082          DISPLAY 'ELFORM IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01083                                                                   
01084      IF ER-FILE-NAME = 'ELFORM'                                   
01085        AND ER-ACTION = 'DELETE'                                   
01086          OPEN I-O ELFORM                                          
01087          PERFORM 1000-ERROR-CHECK                                 
01088          MOVE ALL '9'     TO TX-CONTROL-PRIMARY IN ELFORM         
01089          DELETE ELFORM                                            
01090          PERFORM 1000-ERROR-CHECK                                 
01091          CLOSE ELFORM                                             
01092          PERFORM 1000-ERROR-CHECK                                 
01093          DISPLAY 'ELFORM IS UPDATED (UNLESS NOTED ABOVE)'.        
01094                                                                   
01095      IF ER-FILE-NAME = 'ELHELP'                                   
01096        AND ER-ACTION = 'LOAD'                                     
01097          OPEN OUTPUT ELHELP                                       
01098          PERFORM 1000-ERROR-CHECK                                 
01099          MOVE LOW-VALUES   TO TEXT-FILES         IN ELHELP        
01100          MOVE ALL '9'      TO TX-CONTROL-PRIMARY IN ELHELP        
01101          WRITE TEXT-FILES IN ELHELP                               
01102          PERFORM 1000-ERROR-CHECK                                 
01103          CLOSE ELHELP                                             
01104          PERFORM 1000-ERROR-CHECK                                 
01105          DISPLAY 'ELHELP IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01106                                                                   
01107      IF ER-FILE-NAME = 'ELHELP'                                   
01108        AND ER-ACTION = 'DELETE'                                   
01109          OPEN I-O ELHELP                                          
01110          PERFORM 1000-ERROR-CHECK                                 
01111          MOVE ALL '9'    TO TX-CONTROL-PRIMARY IN ELHELP          
01112          DELETE ELHELP                                            
01113          PERFORM 1000-ERROR-CHECK                                 
01114          CLOSE ELHELP                                             
01115          PERFORM 1000-ERROR-CHECK                                 
01116          DISPLAY 'ELHELP IS UPDATED (UNLESS NOTED ABOVE)'.        
01117  EJECT                                                            
01118      IF ER-FILE-NAME = 'ELLETR'                                   
01119        AND ER-ACTION = 'LOAD'                                     
01120          OPEN OUTPUT ELLETR                                       
01121          PERFORM 1000-ERROR-CHECK                                 
01122          MOVE LOW-VALUES   TO TEXT-FILES         IN ELLETR        
01123          MOVE ALL '9'      TO TX-CONTROL-PRIMARY IN ELLETR        
01124          WRITE TEXT-FILES IN ELLETR                               
01125          PERFORM 1000-ERROR-CHECK                                 
01126          CLOSE ELLETR                                             
01127          PERFORM 1000-ERROR-CHECK                                 
01128          DISPLAY 'ELLETR IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01129                                                                   
01130      IF ER-FILE-NAME = 'ELLETR'                                   
01131        AND ER-ACTION = 'DELETE'                                   
01132          OPEN I-O ELLETR                                          
01133          PERFORM 1000-ERROR-CHECK                                 
01134          MOVE ALL '9'      TO TX-CONTROL-PRIMARY IN ELLETR        
01135          DELETE ELLETR                                            
01136          PERFORM 1000-ERROR-CHECK                                 
01137          CLOSE ELLETR                                             
01138          PERFORM 1000-ERROR-CHECK                                 
01139          DISPLAY 'ELLETR IS UPDATED (UNLESS NOTED ABOVE)'.        
01140                                                                   
01141      IF ER-FILE-NAME = 'ELMSTR'                                   
01142        AND ER-ACTION = 'LOAD'                                     
01143          OPEN OUTPUT ELMSTR                                       
01144          PERFORM 1000-ERROR-CHECK                                 
01145          MOVE LOW-VALUES       TO CLAIM-MASTER       IN ELMSTR    
01146          MOVE ALL '9'          TO CL-CONTROL-PRIMARY IN ELMSTR    
01147          WRITE CLAIM-MASTER IN ELMSTR                             
01148          PERFORM 1000-ERROR-CHECK                                 
01149          CLOSE ELMSTR                                             
01150          PERFORM 1000-ERROR-CHECK                                 
01151          DISPLAY 'ELMSTR IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01152                                                                   
01153      IF ER-FILE-NAME = 'ELMSTR'                                   
01154        AND ER-ACTION = 'DELETE'                                   
01155          OPEN I-O ELMSTR                                          
01156          PERFORM 1000-ERROR-CHECK                                 
01157          MOVE ALL '9'          TO CL-CONTROL-PRIMARY IN ELMSTR    
01158          DELETE ELMSTR                                            
01159          PERFORM 1000-ERROR-CHECK                                 
01160          CLOSE ELMSTR                                             
01161          PERFORM 1000-ERROR-CHECK                                 
01162          DISPLAY 'ELMSTR IS UPDATED (UNLESS NOTED ABOVE)'.        
01163                                                                   
01164      IF ER-FILE-NAME = 'ELRETR'                                   
01165        AND ER-ACTION = 'LOAD'                                     
01166          OPEN OUTPUT ELRETR                                       
01167          PERFORM 1000-ERROR-CHECK                                 
01168          MOVE LOW-VALUES       TO RETRIEVE-MASTER    IN ELRETR    
01169          MOVE ALL '9'          TO RL-CONTROL-PRIMARY IN ELRETR    
01170          WRITE RETRIEVE-MASTER IN ELRETR                          
01171          PERFORM 1000-ERROR-CHECK                                 
01172          CLOSE ELRETR                                             
01173          PERFORM 1000-ERROR-CHECK                                 
01174          DISPLAY 'ELRETR IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01175                                                                   
01176      IF ER-FILE-NAME = 'ELRETR'                                   
01177        AND ER-ACTION = 'DELETE'                                   
01178          OPEN I-O ELRETR                                          
01179          PERFORM 1000-ERROR-CHECK                                 
01180          MOVE ALL '9'          TO RL-CONTROL-PRIMARY IN ELRETR    
01181          DELETE ELRETR                                            
01182          PERFORM 1000-ERROR-CHECK                                 
01183          CLOSE ELRETR                                             
01184          PERFORM 1000-ERROR-CHECK                                 
01185          DISPLAY 'ELRETR IS UPDATED (UNLESS NOTED ABOVE)'.        
01186  EJECT                                                            
01187      IF ER-FILE-NAME = 'ELPGMN'                                   
01188        AND ER-ACTION = 'LOAD'                                     
01189          OPEN OUTPUT ELPGMN                                       
01190          PERFORM 1000-ERROR-CHECK                                 
01191          MOVE LOW-VALUES   TO PROGRAM-DESCRIPTIONS IN ELPGMN      
01192          MOVE ALL '9'      TO PN-CONTROL-PRIMARY   IN ELPGMN      
01193          WRITE PROGRAM-DESCRIPTIONS IN ELPGMN                     
01194          PERFORM 1000-ERROR-CHECK                                 
01195          CLOSE ELPGMN                                             
01196          PERFORM 1000-ERROR-CHECK                                 
01197          DISPLAY 'ELPGMN IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01198                                                                   
01199      IF ER-FILE-NAME = 'ELPGMN'                                   
01200        AND ER-ACTION = 'DELETE'                                   
01201          OPEN I-O ELPGMN                                          
01202          PERFORM 1000-ERROR-CHECK                                 
01203          MOVE ALL '9'     TO PN-CONTROL-PRIMARY IN ELPGMN         
01204          DELETE ELPGMN                                            
01205          PERFORM 1000-ERROR-CHECK                                 
01206          CLOSE ELPGMN                                             
01207          PERFORM 1000-ERROR-CHECK                                 
01208          DISPLAY 'ELPGMN IS UPDATED (UNLESS NOTED ABOVE)'.        
01209                                                                   
01210      IF ER-FILE-NAME = 'ELPGMO'                                   
01211        AND ER-ACTION = 'LOAD'                                     
01212          OPEN OUTPUT ELPGMO                                       
01213          PERFORM 1000-ERROR-CHECK                                 
01214          MOVE LOW-VALUES  TO PROGRAM-OPTIONS-AVAILABLE IN ELPGMO  
01215          MOVE ALL '9'     TO PO-CONTROL-PRIMARY        IN ELPGMO  
01216          WRITE PROGRAM-OPTIONS-AVAILABLE IN ELPGMO                
01217          PERFORM 1000-ERROR-CHECK                                 
01218          CLOSE ELPGMO                                             
01219          PERFORM 1000-ERROR-CHECK                                 
01220          DISPLAY 'ELPGMO IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01221                                                                   
01222      IF ER-FILE-NAME = 'ELPGMO'                                   
01223        AND ER-ACTION = 'DELETE'                                   
01224          OPEN I-O ELPGMO                                          
01225          PERFORM 1000-ERROR-CHECK                                 
01226          MOVE ALL '9'          TO PO-CONTROL-PRIMARY IN ELPGMO    
01227          DELETE ELPGMO                                            
01228          PERFORM 1000-ERROR-CHECK                                 
01229          CLOSE ELPGMO                                             
01230          PERFORM 1000-ERROR-CHECK                                 
01231          DISPLAY 'ELPGMO IS UPDATED (UNLESS NOTED ABOVE)'.        
01232  EJECT                                                            
01233      IF ER-FILE-NAME = 'ELPGMS'                                   
01234        AND ER-ACTION = 'LOAD'                                     
01235          OPEN OUTPUT ELPGMS                                       
01236          PERFORM 1000-ERROR-CHECK                                 
01237          MOVE LOW-VALUES   TO PROGRAM-OPTIONS-SELECTED IN ELPGMS  
01238          MOVE ALL '9'      TO PS-CONTROL-PRIMARY       IN ELPGMS  
01239          WRITE PROGRAM-OPTIONS-SELECTED IN ELPGMS                 
01240          PERFORM 1000-ERROR-CHECK                                 
01241          CLOSE ELPGMS                                             
01242          PERFORM 1000-ERROR-CHECK                                 
01243          DISPLAY 'ELPGMS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01244                                                                   
01245      IF ER-FILE-NAME = 'ELPGMS'                                   
01246        AND ER-ACTION = 'DELETE'                                   
01247          OPEN I-O ELPGMS                                          
01248          PERFORM 1000-ERROR-CHECK                                 
01249          MOVE ALL '9'        TO PS-CONTROL-PRIMARY IN ELPGMS      
01250          DELETE ELPGMS                                            
01251          PERFORM 1000-ERROR-CHECK                                 
01252          CLOSE ELPGMS                                             
01253          PERFORM 1000-ERROR-CHECK                                 
01254          DISPLAY 'ELPGMS IS UPDATED (UNLESS NOTED ABOVE)'.        
01255                                                                   
01256      IF ER-FILE-NAME = 'ELTRLR'                                   
01257        AND ER-ACTION = 'LOAD'                                     
01258          OPEN OUTPUT ELTRLR                                       
01259          PERFORM 1000-ERROR-CHECK                                 
01260          MOVE LOW-VALUES     TO ACTIVITY-TRAILERS  IN ELTRLR      
01261          MOVE ALL '9'        TO AT-CONTROL-PRIMARY IN ELTRLR      
01262          WRITE ACTIVITY-TRAILERS IN ELTRLR                        
01263          PERFORM 1000-ERROR-CHECK                                 
01264          CLOSE ELTRLR                                             
01265          PERFORM 1000-ERROR-CHECK                                 
01266          DISPLAY 'ELTRLR IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01267                                                                   
01268      IF ER-FILE-NAME = 'ELTRLR'                                   
01269        AND ER-ACTION = 'DELETE'                                   
01270          OPEN I-O ELTRLR                                          
01271          PERFORM 1000-ERROR-CHECK                                 
01272          MOVE ALL '9'      TO AT-CONTROL-PRIMARY IN ELTRLR        
01273          DELETE ELTRLR                                            
01274          PERFORM 1000-ERROR-CHECK                                 
01275          CLOSE ELTRLR                                             
01276          PERFORM 1000-ERROR-CHECK                                 
01277          DISPLAY 'ELTRLR IS UPDATED (UNLESS NOTED ABOVE)'.        
01278  EJECT                                                            
01279      IF ER-FILE-NAME = 'ELREPT'                                   
01280        AND ER-ACTION = 'LOAD'                                     
01281          OPEN OUTPUT ELREPT                                       
01282          PERFORM 1000-ERROR-CHECK                                 
01283          MOVE LOW-VALUES          TO REPORT-SAVE-FILE   IN ELREPT 
01284          MOVE ALL '9'             TO RF-CONTROL-PRIMARY IN ELREPT 
01285          WRITE REPORT-SAVE-FILE IN ELREPT                         
01286          PERFORM 1000-ERROR-CHECK                                 
01287          CLOSE ELREPT                                             
01288          PERFORM 1000-ERROR-CHECK                                 
01289          DISPLAY 'ELREPT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01290                                                                   
01291      IF ER-FILE-NAME = 'ELREPT'                                   
01292        AND ER-ACTION = 'DELETE'                                   
01293          OPEN I-O ELREPT                                          
01294          PERFORM 1000-ERROR-CHECK                                 
01295          MOVE ALL '9'        TO RF-CONTROL-PRIMARY IN ELREPT      
01296          DELETE ELREPT                                            
01297          PERFORM 1000-ERROR-CHECK                                 
01298          CLOSE ELREPT                                             
01299          PERFORM 1000-ERROR-CHECK                                 
01300          DISPLAY 'ELREPT IS UPDATED (UNLESS NOTED ABOVE)'.        
01301                                                                   
01302      IF ER-FILE-NAME = 'ERACCT'                                   
01303        AND ER-ACTION = 'LOAD'                                     
01304          OPEN OUTPUT ERACCT                                       
01305          PERFORM 1000-ERROR-CHECK                                 
01306          MOVE LOW-VALUES   TO ACCOUNT-MASTER     IN ERACCT        
01307          MOVE ALL '9'      TO AM-CONTROL-PRIMARY IN ERACCT        
01308          WRITE ACCOUNT-MASTER IN ERACCT                           
01309          PERFORM 1000-ERROR-CHECK                                 
01310          CLOSE ERACCT                                             
01311          PERFORM 1000-ERROR-CHECK                                 
01312          DISPLAY 'ERACCT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01313                                                                   
01314      IF ER-FILE-NAME = 'ERACCT'                                   
01315        AND ER-ACTION = 'DELETE'                                   
01316          OPEN I-O ERACCT                                          
01317          PERFORM 1000-ERROR-CHECK                                 
01318          MOVE ALL '9'      TO AM-CONTROL-PRIMARY IN ERACCT        
01319          DELETE ERACCT                                            
01320          PERFORM 1000-ERROR-CHECK                                 
01321          CLOSE ERACCT                                             
01322          PERFORM 1000-ERROR-CHECK                                 
01323          DISPLAY 'ERACCT IS UPDATED (UNLESS NOTED ABOVE)'.        
01324  EJECT                                                            
01325      IF ER-FILE-NAME = 'ERACNT'                                   
01326        AND ER-ACTION = 'LOAD'                                     
01327          OPEN OUTPUT ERACNT                                       
01328          PERFORM 1000-ERROR-CHECK                                 
01329          MOVE LOW-VALUES   TO NOTE-FILE          IN ERACNT        
01330          MOVE ALL '9'      TO NT-CONTROL-PRIMARY IN ERACNT        
01331          WRITE NOTE-FILE IN ERACNT                                
01332          PERFORM 1000-ERROR-CHECK                                 
01333          CLOSE ERACNT                                             
01334          PERFORM 1000-ERROR-CHECK                                 
01335          DISPLAY 'ERACNT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01336                                                                   
01337      IF ER-FILE-NAME = 'ERACNT'                                   
01338        AND ER-ACTION = 'DELETE'                                   
01339          OPEN I-O ERACNT                                          
01340          PERFORM 1000-ERROR-CHECK                                 
01341          MOVE ALL '9'      TO NT-CONTROL-PRIMARY IN ERACNT        
01342          DELETE ERACNT                                            
01343          PERFORM 1000-ERROR-CHECK                                 
01344          CLOSE ERACNT                                             
01345          PERFORM 1000-ERROR-CHECK                                 
01346          DISPLAY 'ERACNT IS UPDATED (UNLESS NOTED ABOVE)'.        
01347  EJECT                                                            
01348      IF ER-FILE-NAME = 'ERARCH'                                   
01349        AND ER-ACTION = 'LOAD'                                     
01350          OPEN OUTPUT ERARCH                                       
01351          PERFORM 1000-ERROR-CHECK                                 
01352          MOVE LOW-VALUES   TO LETTER-ARCHIVE     IN ERARCH        
01353          MOVE ALL '9'      TO LA-CONTROL-PRIMARY IN ERARCH        
01354          WRITE LETTER-ARCHIVE IN ERARCH                           
01355          PERFORM 1000-ERROR-CHECK                                 
01356          CLOSE ERARCH                                             
01357          PERFORM 1000-ERROR-CHECK                                 
01358          DISPLAY 'ERARCH IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01359                                                                   
01360      IF ER-FILE-NAME = 'ERARCH'                                   
01361        AND ER-ACTION = 'DELETE'                                   
01362          OPEN I-O ERARCH                                          
01363          PERFORM 1000-ERROR-CHECK                                 
01364          MOVE ALL '9'      TO LA-CONTROL-PRIMARY IN ERARCH        
01365          DELETE ERARCH                                            
01366          PERFORM 1000-ERROR-CHECK                                 
01367          CLOSE ERARCH                                             
01368          PERFORM 1000-ERROR-CHECK                                 
01369          DISPLAY 'ERARCH IS UPDATED (UNLESS NOTED ABOVE)'.        
01370  EJECT                                                            
01371      IF ER-FILE-NAME = 'ERARCT'                                   
01372        AND ER-ACTION = 'LOAD'                                     
01373          OPEN OUTPUT ERARCT                                       
01374          PERFORM 1000-ERROR-CHECK                                 
01375          MOVE LOW-VALUES   TO LETTER-ARCHIVE-TEXT IN ERARCT       
01376          MOVE ALL '9'      TO LT-CONTROL-PRIMARY  IN ERARCT       
01377          WRITE LETTER-ARCHIVE-TEXT IN ERARCT                      
01378          PERFORM 1000-ERROR-CHECK                                 
01379          CLOSE ERARCT                                             
01380          PERFORM 1000-ERROR-CHECK                                 
01381          DISPLAY 'ERARCT IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01382                                                                   
01383      IF ER-FILE-NAME = 'ERARCT'                                   
01384        AND ER-ACTION = 'DELETE'                                   
01385          OPEN I-O ERARCT                                          
01386          PERFORM 1000-ERROR-CHECK                                 
01387          MOVE ALL '9'      TO LT-CONTROL-PRIMARY IN ERARCT        
01388          DELETE ERARCT                                            
01389          PERFORM 1000-ERROR-CHECK                                 
01390          CLOSE ERARCT                                             
01391          PERFORM 1000-ERROR-CHECK                                 
01392          DISPLAY 'ERARCT IS UPDATED (UNLESS NOTED ABOVE)'.        
01393  EJECT                                                            
01394      IF ER-FILE-NAME = 'ERBILL'                                   
01395        AND ER-ACTION = 'LOAD'                                     
01396          OPEN OUTPUT ERBILL                                       
01397          PERFORM 1000-ERROR-CHECK                                 
01398          MOVE LOW-VALUES    TO BILLING-STATEMENT  IN ERBILL       
01399          MOVE ALL '9'       TO BI-CONTROL-PRIMARY IN ERBILL       
01400          WRITE BILLING-STATEMENT IN ERBILL                        
01401          PERFORM 1000-ERROR-CHECK                                 
01402          CLOSE ERBILL                                             
01403          PERFORM 1000-ERROR-CHECK                                 
01404          DISPLAY 'ERBILL IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01405                                                                   
01406      IF ER-FILE-NAME = 'ERBILL'                                   
01407        AND ER-ACTION = 'DELETE'                                   
01408          OPEN I-O ERBILL                                          
01409          PERFORM 1000-ERROR-CHECK                                 
01410          MOVE ALL '9'      TO BI-CONTROL-PRIMARY IN ERBILL        
01411          DELETE ERBILL                                            
01412          PERFORM 1000-ERROR-CHECK                                 
01413          CLOSE ERBILL                                             
01414          PERFORM 1000-ERROR-CHECK                                 
01415          DISPLAY 'ERBILL IS UPDATED (UNLESS NOTED ABOVE)'.        
01416                                                                   
01417      IF ER-FILE-NAME = 'ERCHEK'                                   
01418        AND ER-ACTION = 'LOAD'                                     
01419          OPEN OUTPUT ERCHEK                                       
01420          PERFORM 1000-ERROR-CHECK                                 
01421          MOVE LOW-VALUES   TO CHECK-RECORDS      IN ERCHEK        
01422          MOVE ALL '9'      TO CH-CONTROL-PRIMARY IN ERCHEK        
01423          WRITE CHECK-RECORDS IN ERCHEK                            
01424          PERFORM 1000-ERROR-CHECK                                 
01425          CLOSE ERCHEK                                             
01426          PERFORM 1000-ERROR-CHECK                                 
01427          DISPLAY 'ERCHEK IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01428                                                                   
01429      IF ER-FILE-NAME = 'ERCHEK'                                   
01430        AND ER-ACTION = 'DELETE'                                   
01431          OPEN I-O ERCHEK                                          
01432          PERFORM 1000-ERROR-CHECK                                 
01433          MOVE ALL '9'       TO CH-CONTROL-PRIMARY IN ERCHEK       
01434          DELETE ERCHEK                                            
01435          PERFORM 1000-ERROR-CHECK                                 
01436          CLOSE ERCHEK                                             
01437          PERFORM 1000-ERROR-CHECK                                 
01438          DISPLAY 'ERCHEK IS UPDATED (UNLESS NOTED ABOVE)'.        
01439  EJECT                                                            
01440      IF ER-FILE-NAME = 'ERCHKQ'                                   
01441        AND ER-ACTION = 'LOAD'                                     
01442          OPEN OUTPUT ERCHKQ                                       
01443          PERFORM 1000-ERROR-CHECK                                 
01444          MOVE LOW-VALUES     TO CHECK-QUE          IN ERCHKQ      
01445          MOVE ALL '9'        TO CQ-CONTROL-PRIMARY IN ERCHKQ      
01446          WRITE CHECK-QUE IN ERCHKQ                                
01447          PERFORM 1000-ERROR-CHECK                                 
01448          CLOSE ERCHKQ                                             
01449          PERFORM 1000-ERROR-CHECK                                 
01450          DISPLAY 'ERCHKQ IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01451                                                                   
01452      IF ER-FILE-NAME = 'ERCHKQ'                                   
01453        AND ER-ACTION = 'DELETE'                                   
01454          OPEN I-O ERCHKQ                                          
01455          PERFORM 1000-ERROR-CHECK                                 
01456          MOVE ALL '9'       TO CQ-CONTROL-PRIMARY IN ERCHKQ       
01457          DELETE ERCHKQ                                            
01458          PERFORM 1000-ERROR-CHECK                                 
01459          CLOSE ERCHKQ                                             
01460          PERFORM 1000-ERROR-CHECK                                 
01461          DISPLAY 'ERCHKQ IS UPDATED (UNLESS NOTED ABOVE)'.        
01462                                                                   
01463      IF ER-FILE-NAME = 'ERCOMP'                                   
01464        AND ER-ACTION = 'LOAD'                                     
01465          OPEN OUTPUT ERCOMP                                       
01466          PERFORM 1000-ERROR-CHECK                                 
01467          MOVE LOW-VALUES   TO COMPENSATION-MASTER IN ERCOMP       
01468          MOVE ALL '9'      TO CO-CONTROL-PRIMARY  IN ERCOMP       
01469          WRITE COMPENSATION-MASTER IN ERCOMP                      
01470          PERFORM 1000-ERROR-CHECK                                 
01471          CLOSE ERCOMP                                             
01472          PERFORM 1000-ERROR-CHECK                                 
01473          DISPLAY 'ERCOMP IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01474                                                                   
01475      IF ER-FILE-NAME = 'ERCOMP'                                   
01476        AND ER-ACTION = 'DELETE'                                   
01477          OPEN I-O ERCOMP                                          
01478          PERFORM 1000-ERROR-CHECK                                 
01479          MOVE ALL '9'        TO CO-CONTROL-PRIMARY IN ERCOMP      
01480          DELETE ERCOMP                                            
01481          PERFORM 1000-ERROR-CHECK                                 
01482          CLOSE ERCOMP                                             
01483          PERFORM 1000-ERROR-CHECK                                 
01484          DISPLAY 'ERCOMP IS UPDATED (UNLESS NOTED ABOVE)'.        
01485  EJECT                                                            
01486      IF ER-FILE-NAME = 'ERCRTC'                                   
01487        AND ER-ACTION = 'LOAD'                                     
01488          OPEN OUTPUT ERCRTC                                       
01489          PERFORM 1000-ERROR-CHECK                                 
01490          MOVE LOW-VALUES  TO PENDING-MAINT-TO-CERT-FILE IN ERCRTC 
01491          MOVE ALL '9'     TO CC-CONTROL-PRIMARY         IN ERCRTC 
01492          WRITE PENDING-MAINT-TO-CERT-FILE IN ERCRTC               
01493          PERFORM 1000-ERROR-CHECK                                 
01494          CLOSE ERCRTC                                             
01495          PERFORM 1000-ERROR-CHECK                                 
01496          DISPLAY 'ERCRTC IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01497                                                                   
01498      IF ER-FILE-NAME = 'ERCRTC'                                   
01499        AND ER-ACTION = 'DELETE'                                   
01500          OPEN I-O ERCRTC                                          
01501          PERFORM 1000-ERROR-CHECK                                 
01502          MOVE ALL '9'        TO CC-CONTROL-PRIMARY IN ERCRTC      
01503          DELETE ERCRTC                                            
01504          PERFORM 1000-ERROR-CHECK                                 
01505          CLOSE ERCRTC                                             
01506          PERFORM 1000-ERROR-CHECK                                 
01507          DISPLAY 'ERCRTC IS UPDATED (UNLESS NOTED ABOVE)'.        
01508                                                                   
01509      IF ER-FILE-NAME = 'ERCTBL'                                   
01510        AND ER-ACTION = 'LOAD'                                     
01511          OPEN OUTPUT ERCTBL                                       
01512          PERFORM 1000-ERROR-CHECK                                 
01513          MOVE LOW-VALUES       TO COMM-TABLE-RECORD  IN ERCTBL    
01514          MOVE ALL '9'          TO CT-CONTROL-PRIMARY IN ERCTBL    
01515          WRITE COMM-TABLE-RECORD IN ERCTBL                        
01516          PERFORM 1000-ERROR-CHECK                                 
01517          CLOSE ERCTBL                                             
01518          PERFORM 1000-ERROR-CHECK                                 
01519          DISPLAY 'ERCTBL IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01520                                                                   
01521      IF ER-FILE-NAME = 'ERCTBL'                                   
01522        AND ER-ACTION = 'DELETE'                                   
01523          OPEN I-O ERCTBL                                          
01524          PERFORM 1000-ERROR-CHECK                                 
01525          MOVE ALL '9'         TO CT-CONTROL-PRIMARY IN ERCTBL     
01526          DELETE ERCTBL                                            
01527          PERFORM 1000-ERROR-CHECK                                 
01528          CLOSE ERCTBL                                             
01529          PERFORM 1000-ERROR-CHECK                                 
01530          DISPLAY 'ERCTBL IS UPDATED (UNLESS NOTED ABOVE)'.        
01531  EJECT                                                            
01532      IF ER-FILE-NAME = 'ERNAME'                                   
01533        AND ER-ACTION = 'LOAD'                                     
01534          OPEN OUTPUT ERNAME                                       
01535          PERFORM 1000-ERROR-CHECK                                 
01536          MOVE LOW-VALUES       TO NAME-LOOKUP-MASTER IN ERNAME    
01537          MOVE ALL '9'          TO NL-CONTROL-PRIMARY IN ERNAME    
01538          WRITE NAME-LOOKUP-MASTER IN ERNAME                       
01539          PERFORM 1000-ERROR-CHECK                                 
01540          CLOSE ERNAME                                             
01541          PERFORM 1000-ERROR-CHECK                                 
01542          DISPLAY 'ERNAME IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01543                                                                   
01544      IF ER-FILE-NAME = 'ERNAME'                                   
01545        AND ER-ACTION = 'DELETE'                                   
01546          OPEN I-O ERNAME                                          
01547          PERFORM 1000-ERROR-CHECK                                 
01548          MOVE ALL '9'         TO NL-CONTROL-PRIMARY IN ERNAME     
01549          DELETE ERNAME                                            
01550          PERFORM 1000-ERROR-CHECK                                 
01551          CLOSE ERNAME                                             
01552          PERFORM 1000-ERROR-CHECK                                 
01553          DISPLAY 'ERNAME IS UPDATED (UNLESS NOTED ABOVE)'.        
01554                                                                   
01555      IF ER-FILE-NAME = 'ERPLAN'                                   
01556        AND ER-ACTION = 'LOAD'                                     
01557          OPEN OUTPUT ERPLAN                                       
01558          PERFORM 1000-ERROR-CHECK                                 
01559          MOVE LOW-VALUES          TO PLAN-MASTER        IN ERPLAN 
01560          MOVE ALL '9'             TO PL-CONTROL-PRIMARY IN ERPLAN 
01561          WRITE PLAN-MASTER IN ERPLAN                              
01562          PERFORM 1000-ERROR-CHECK                                 
01563          CLOSE ERPLAN                                             
01564          PERFORM 1000-ERROR-CHECK                                 
01565          DISPLAY 'ERPLAN IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01566                                                                   
01567      IF ER-FILE-NAME = 'ERPLAN'                                   
01568        AND ER-ACTION = 'DELETE'                                   
01569          OPEN I-O ERPLAN                                          
01570          PERFORM 1000-ERROR-CHECK                                 
01571          MOVE ALL '9'     TO PL-CONTROL-PRIMARY IN ERPLAN         
01572          DELETE ERPLAN                                            
01573          PERFORM 1000-ERROR-CHECK                                 
01574          CLOSE ERPLAN                                             
01575          PERFORM 1000-ERROR-CHECK                                 
01576          DISPLAY 'ERPLAN IS UPDATED (UNLESS NOTED ABOVE)'.        
01577  EJECT                                                            
01578      IF ER-FILE-NAME = 'ERPNDB'                                   
01579        AND ER-ACTION = 'LOAD'                                     
01580          OPEN OUTPUT ERPNDB                                       
01581          PERFORM 1000-ERROR-CHECK                                 
01582          MOVE LOW-VALUES          TO PENDING-BUSINESS   IN ERPNDB 
01583          MOVE ALL '9'             TO PB-CONTROL-PRIMARY IN ERPNDB 
01584          WRITE PENDING-BUSINESS IN ERPNDB                         
01585          PERFORM 1000-ERROR-CHECK                                 
01586          CLOSE ERPNDB                                             
01587          PERFORM 1000-ERROR-CHECK                                 
01588          DISPLAY 'ERPNDB IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01589                                                                   
01590      IF ER-FILE-NAME = 'ERPNDB'                                   
01591        AND ER-ACTION = 'DELETE'                                   
01592          OPEN I-O ERPNDB                                          
01593          PERFORM 1000-ERROR-CHECK                                 
01594          MOVE ALL '9'       TO PB-CONTROL-PRIMARY IN ERPNDB       
01595          DELETE ERPNDB                                            
01596          PERFORM 1000-ERROR-CHECK                                 
01597          CLOSE ERPNDB                                             
01598          PERFORM 1000-ERROR-CHECK                                 
01599          DISPLAY 'ERPNDB IS UPDATED (UNLESS NOTED ABOVE)'.        
01600                                                                   
01601  EJECT                                                            
01602      IF ER-FILE-NAME = 'ERPNDE'                                   
01603        AND ER-ACTION = 'LOAD'                                     
01604          OPEN OUTPUT ERPNDE                                       
01605          PERFORM 1000-ERROR-CHECK                                 
01606          MOVE LOW-VALUES          TO PENDING-BUSINESS   IN ERPNDE 
01607          MOVE ALL '9'             TO PB-CONTROL-PRIMARY IN ERPNDE 
01608          WRITE PENDING-BUSINESS IN ERPNDE                         
01609          PERFORM 1000-ERROR-CHECK                                 
01610          CLOSE ERPNDE                                             
01611          PERFORM 1000-ERROR-CHECK                                 
01612          DISPLAY 'ERPNDE IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01613                                                                   
01614      IF ER-FILE-NAME = 'ERPNDE'                                   
01615        AND ER-ACTION = 'DELETE'                                   
01616          OPEN I-O ERPNDE                                          
01617          PERFORM 1000-ERROR-CHECK                                 
01618          MOVE ALL '9'       TO PB-CONTROL-PRIMARY IN ERPNDE       
01619          DELETE ERPNDE                                            
01620          PERFORM 1000-ERROR-CHECK                                 
01621          CLOSE ERPNDE                                             
01622          PERFORM 1000-ERROR-CHECK                                 
01623          DISPLAY 'ERPNDE IS UPDATED (UNLESS NOTED ABOVE)'.        
01624                                                                   
01625      IF ER-FILE-NAME = 'ERPNDC'                                   
01626        AND ER-ACTION = 'LOAD'                                     
01627          OPEN OUTPUT ERPNDC                                       
01628          PERFORM 1000-ERROR-CHECK                                 
01629          MOVE LOW-VALUES          TO PENDING-CLAIMS     IN ERPNDC 
01630          MOVE ALL '9'             TO PC-CONTROL-PRIMARY IN ERPNDC 
01631          WRITE PENDING-CLAIMS IN ERPNDC                           
01632          PERFORM 1000-ERROR-CHECK                                 
01633          CLOSE ERPNDC                                             
01634          PERFORM 1000-ERROR-CHECK                                 
01635          DISPLAY 'ERPNDC IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01636                                                                   
01637      IF ER-FILE-NAME = 'ERPNDC'                                   
01638        AND ER-ACTION = 'DELETE'                                   
01639          OPEN I-O ERPNDC                                          
01640          PERFORM 1000-ERROR-CHECK                                 
01641          MOVE ALL '9'           TO PC-CONTROL-PRIMARY IN ERPNDC   
01642          DELETE ERPNDC                                            
01643          PERFORM 1000-ERROR-CHECK                                 
01644          CLOSE ERPNDC                                             
01645          PERFORM 1000-ERROR-CHECK                                 
01646          DISPLAY 'ERPNDC IS UPDATED (UNLESS NOTED ABOVE)'.        
01647  EJECT                                                            
01648      IF ER-FILE-NAME = 'ERRATE'                                   
01649        AND ER-ACTION = 'LOAD'                                     
01650          OPEN OUTPUT ERRATE                                       
01651          PERFORM 1000-ERROR-CHECK                                 
01652          MOVE LOW-VALUES          TO RATE-RECORD        IN ERRATE 
01653          MOVE ALL '9'             TO RT-CONTROL-PRIMARY IN ERRATE 
01654          WRITE RATE-RECORD IN ERRATE                              
01655          PERFORM 1000-ERROR-CHECK                                 
01656          CLOSE ERRATE                                             
01657          PERFORM 1000-ERROR-CHECK                                 
01658          DISPLAY 'ERRATE IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01659                                                                   
01660      IF ER-FILE-NAME = 'ERRATE'                                   
01661        AND ER-ACTION = 'DELETE'                                   
01662          OPEN I-O ERRATE                                          
01663          PERFORM 1000-ERROR-CHECK                                 
01664          MOVE ALL '9'       TO RT-CONTROL-PRIMARY IN ERRATE       
01665          DELETE ERRATE                                            
01666          PERFORM 1000-ERROR-CHECK                                 
01667          CLOSE ERRATE                                             
01668          PERFORM 1000-ERROR-CHECK                                 
01669          DISPLAY 'ERRATE IS UPDATED (UNLESS NOTED ABOVE)'.        
01670                                                                   
01671      IF ER-FILE-NAME = 'ERREIN'                                   
01672        AND ER-ACTION = 'LOAD'                                     
01673          OPEN OUTPUT ERREIN                                       
01674          PERFORM 1000-ERROR-CHECK                                 
01675          MOVE LOW-VALUES      TO REINSURANCE-RECORD IN ERREIN     
01676          MOVE ALL '9'         TO RE-CONTROL-PRIMARY IN ERREIN     
01677          WRITE REINSURANCE-RECORD IN ERREIN                       
01678          PERFORM 1000-ERROR-CHECK                                 
01679          CLOSE ERREIN                                             
01680          PERFORM 1000-ERROR-CHECK                                 
01681          DISPLAY 'ERREIN IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01682                                                                   
01683      IF ER-FILE-NAME = 'ERREIN'                                   
01684        AND ER-ACTION = 'DELETE'                                   
01685          OPEN I-O ERREIN                                          
01686          PERFORM 1000-ERROR-CHECK                                 
01687          MOVE ALL '9'       TO RE-CONTROL-PRIMARY IN ERREIN       
01688          DELETE ERREIN                                            
01689          PERFORM 1000-ERROR-CHECK                                 
01690          CLOSE ERREIN                                             
01691          PERFORM 1000-ERROR-CHECK                                 
01692          DISPLAY 'ERREIN IS UPDATED (UNLESS NOTED ABOVE)'.        
01693  EJECT                                                            
01694      IF ER-FILE-NAME = 'ERREPY'                                   
01695        AND ER-ACTION = 'LOAD'                                     
01696          OPEN OUTPUT ERREPY                                       
01697          PERFORM 1000-ERROR-CHECK                                 
01698          MOVE LOW-VALUES                                          
01699                      TO PENDING-RETRO-REIN-ADJUSTMENTS IN ERREPY  
01700          MOVE ALL '9'          TO RP-CONTROL-PRIMARY   IN ERREPY  
01701          WRITE PENDING-RETRO-REIN-ADJUSTMENTS IN ERREPY           
01702          PERFORM 1000-ERROR-CHECK                                 
01703          CLOSE ERREPY                                             
01704          PERFORM 1000-ERROR-CHECK                                 
01705          DISPLAY 'ERREPY IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01706                                                                   
01707      IF ER-FILE-NAME = 'ERREPY'                                   
01708        AND ER-ACTION = 'DELETE'                                   
01709          OPEN I-O ERREPY                                          
01710          PERFORM 1000-ERROR-CHECK                                 
01711          MOVE ALL '9'            TO RP-CONTROL-PRIMARY IN ERREPY  
01712          DELETE ERREPY                                            
01713          PERFORM 1000-ERROR-CHECK                                 
01714          CLOSE ERREPY                                             
01715          PERFORM 1000-ERROR-CHECK                                 
01716          DISPLAY 'ERREPY IS UPDATED (UNLESS NOTED ABOVE)'.        
01717                                                                   
01718      IF ER-FILE-NAME = 'ERPYAJ'                                   
01719        AND ER-ACTION = 'LOAD'                                     
01720          OPEN OUTPUT ERPYAJ                                       
01721          PERFORM 1000-ERROR-CHECK                                 
01722          MOVE LOW-VALUES    TO PENDING-PAY-ADJ    IN ERPYAJ       
01723          MOVE ALL '9'       TO PY-CONTROL-PRIMARY IN ERPYAJ       
01724          WRITE PENDING-PAY-ADJ IN ERPYAJ                          
01725          PERFORM 1000-ERROR-CHECK                                 
01726          CLOSE ERPYAJ                                             
01727          PERFORM 1000-ERROR-CHECK                                 
01728          DISPLAY 'ERPYAJ IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01729                                                                   
01730      IF ER-FILE-NAME = 'ERPYAJ'                                   
01731        AND ER-ACTION = 'DELETE'                                   
01732          OPEN I-O ERPYAJ                                          
01733          PERFORM 1000-ERROR-CHECK                                 
01734          MOVE ALL '9'      TO PY-CONTROL-PRIMARY IN ERPYAJ        
01735          DELETE ERPYAJ                                            
01736          PERFORM 1000-ERROR-CHECK                                 
01737          CLOSE ERPYAJ                                             
01738          PERFORM 1000-ERROR-CHECK                                 
01739          DISPLAY 'ERPYAJ IS UPDATED (UNLESS NOTED ABOVE)'.        
01740  EJECT                                                            
01741      IF ER-FILE-NAME = 'ERNOTE'                                   
01742        AND ER-ACTION = 'LOAD'                                     
01743          OPEN OUTPUT ERNOTE                                       
01744          PERFORM 1000-ERROR-CHECK                                 
01745          MOVE LOW-VALUES      TO CERTIFICATE-NOTE   IN ERNOTE     
01746          MOVE ALL '9'         TO CN-CONTROL-PRIMARY IN ERNOTE     
01747          WRITE CERTIFICATE-NOTE IN ERNOTE                         
01748          PERFORM 1000-ERROR-CHECK                                 
01749          CLOSE ERNOTE                                             
01750          PERFORM 1000-ERROR-CHECK                                 
01751          DISPLAY 'ERNOTE IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01752                                                                   
01753      IF ER-FILE-NAME = 'ERNOTE'                                   
01754        AND ER-ACTION = 'DELETE'                                   
01755          OPEN I-O ERNOTE                                          
01756          PERFORM 1000-ERROR-CHECK                                 
01757          MOVE ALL '9'         TO CN-CONTROL-PRIMARY IN ERNOTE     
01758          DELETE ERNOTE                                            
01759          PERFORM 1000-ERROR-CHECK                                 
01760          CLOSE ERNOTE                                             
01761          PERFORM 1000-ERROR-CHECK                                 
01762          DISPLAY 'ERNOTE IS UPDATED (UNLESS NOTED ABOVE)'.        
01763                                                                   
01764      IF ER-FILE-NAME = 'ERGXRF'                                   
01765        AND ER-ACTION = 'LOAD'                                     
01766          OPEN OUTPUT ERGXRF                                       
01767          PERFORM 1000-ERROR-CHECK                                 
01768          MOVE LOW-VALUES      TO AGENT-CROSS-REFERENCE IN ERGXRF  
01769      (1:LENGTH OF AGENT-CROSS-REFERENCE OF ERGXRF)                
01770          MOVE ALL '9'         TO GX-CONTROL-PRIMARY    IN ERGXRF  
01771          MOVE +1              TO GX-AGENT-POINTER-CNT             
01772          WRITE AGENT-CROSS-REFERENCE IN ERGXRF                    
01773          PERFORM 1000-ERROR-CHECK                                 
01774          CLOSE ERGXRF                                             
01775          PERFORM 1000-ERROR-CHECK                                 
01776          DISPLAY 'ERGXRF IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01777                                                                   
01778      IF ER-FILE-NAME = 'ERGXRF'                                   
01779        AND ER-ACTION = 'DELETE'                                   
01780          OPEN I-O ERGXRF                                          
01781          PERFORM 1000-ERROR-CHECK                                 
01782          MOVE ALL '9'         TO GX-CONTROL-PRIMARY IN ERGXRF     
01783          DELETE ERGXRF                                            
01784          PERFORM 1000-ERROR-CHECK                                 
01785          CLOSE ERGXRF                                             
01786          PERFORM 1000-ERROR-CHECK                                 
01787          DISPLAY 'ERGXRF IS UPDATED (UNLESS NOTED ABOVE)'.        
01788  EJECT                                                            
01789      IF ER-FILE-NAME = 'ERCOMM'                                   
01790        AND ER-ACTION = 'LOAD'                                     
01791          OPEN OUTPUT ERCOMM                                       
01792          PERFORM 1000-ERROR-CHECK                                 
01793          MOVE LOW-VALUES     TO COMMISSION-EXCEPTIONS IN ERCOMM   
01794          MOVE ALL '9'        TO CE-CONTROL-PRIMARY    IN ERCOMM   
01795          WRITE COMMISSION-EXCEPTIONS IN ERCOMM                    
01796          PERFORM 1000-ERROR-CHECK                                 
01797          CLOSE ERCOMM                                             
01798          PERFORM 1000-ERROR-CHECK                                 
01799          DISPLAY 'ERCOMM IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01800                                                                   
01801      IF ER-FILE-NAME = 'ERCOMM'                                   
01802        AND ER-ACTION = 'DELETE'                                   
01803          OPEN I-O ERCOMM                                          
01804          PERFORM 1000-ERROR-CHECK                                 
01805          MOVE ALL '9'         TO CE-CONTROL-PRIMARY IN ERCOMM     
01806          DELETE ERCOMM                                            
01807          PERFORM 1000-ERROR-CHECK                                 
01808          CLOSE ERCOMM                                             
01809          PERFORM 1000-ERROR-CHECK                                 
01810          DISPLAY 'ERCOMM IS UPDATED (UNLESS NOTED ABOVE)'.        
01811                                                                   
01812      IF ER-FILE-NAME = 'ERLOFC'                                   
01813        AND ER-ACTION = 'LOAD'                                     
01814          OPEN OUTPUT ERLOFC                                       
01815          PERFORM 1000-ERROR-CHECK                                 
01816          MOVE LOW-VALUES        TO LOAN-OFFICER-MASTER  IN ERLOFC 
01817          MOVE ALL '9'           TO LO-CONTROL-PRIMARY   IN ERLOFC 
01818          WRITE LOAN-OFFICER-MASTER IN ERLOFC                      
01819          PERFORM 1000-ERROR-CHECK                                 
01820          CLOSE ERLOFC                                             
01821          PERFORM 1000-ERROR-CHECK                                 
01822          DISPLAY 'ERLOFC IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01823                                                                   
01824      IF ER-FILE-NAME = 'ERLOFC'                                   
01825        AND ER-ACTION = 'DELETE'                                   
01826          OPEN I-O ERLOFC                                          
01827          PERFORM 1000-ERROR-CHECK                                 
01828          MOVE ALL '9'        TO LO-CONTROL-PRIMARY IN ERLOFC      
01829          DELETE ERLOFC                                            
01830          PERFORM 1000-ERROR-CHECK                                 
01831          CLOSE ERLOFC                                             
01832          PERFORM 1000-ERROR-CHECK                                 
01833          DISPLAY 'ERLOFC IS UPDATED (UNLESS NOTED ABOVE)'.        
01834  EJECT                                                            
01835      IF ER-FILE-NAME = 'ERMEBL'                                   
01836        AND ER-ACTION = 'LOAD'                                     
01837          OPEN OUTPUT ERMEBL                                       
01838          PERFORM 1000-ERROR-CHECK                                 
01839          MOVE LOW-VALUES         TO MONTH-END-BALANCES IN ERMEBL  
01840          MOVE ALL '9'            TO ME-CONTROL-PRIMARY IN ERMEBL  
01841          WRITE MONTH-END-BALANCES IN ERMEBL                       
01842          PERFORM 1000-ERROR-CHECK                                 
01843          CLOSE ERMEBL                                             
01844          PERFORM 1000-ERROR-CHECK                                 
01845          DISPLAY 'ERMEBL IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01846                                                                   
01847      IF ER-FILE-NAME = 'ERMEBL'                                   
01848        AND ER-ACTION = 'DELETE'                                   
01849          OPEN I-O ERMEBL                                          
01850          PERFORM 1000-ERROR-CHECK                                 
01851          MOVE ALL '9'          TO ME-CONTROL-PRIMARY IN ERMEBL    
01852          DELETE ERMEBL                                            
01853          PERFORM 1000-ERROR-CHECK                                 
01854          CLOSE ERMEBL                                             
01855          PERFORM 1000-ERROR-CHECK                                 
01856          DISPLAY 'ERMEBL IS UPDATED (UNLESS NOTED ABOVE)'.        
01857                                                                   
01858      IF ER-FILE-NAME = 'ERMAIL'                                   
01859        AND ER-ACTION = 'LOAD'                                     
01860          OPEN OUTPUT ERMAIL                                       
01861          PERFORM 1000-ERROR-CHECK                                 
01862          MOVE LOW-VALUES         TO MAILING-DATA       IN ERMAIL  
01863          MOVE ALL '9'            TO MA-CONTROL-PRIMARY IN ERMAIL  
01864          WRITE MAILING-DATA IN ERMAIL                             
01865          PERFORM 1000-ERROR-CHECK                                 
01866          CLOSE ERMAIL                                             
01867          PERFORM 1000-ERROR-CHECK                                 
01868          DISPLAY 'ERMAIL IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01869                                                                   
01870      IF ER-FILE-NAME = 'ERMAIL'                                   
01871        AND ER-ACTION = 'DELETE'                                   
01872          OPEN I-O ERMAIL                                          
01873          PERFORM 1000-ERROR-CHECK                                 
01874          MOVE ALL '9'       TO MA-CONTROL-PRIMARY IN ERMAIL       
01875          DELETE ERMAIL                                            
01876          PERFORM 1000-ERROR-CHECK                                 
01877          CLOSE ERMAIL                                             
01878          PERFORM 1000-ERROR-CHECK                                 
01879          DISPLAY 'ERMAIL IS UPDATED (UNLESS NOTED ABOVE)'.        
01880  EJECT                                                            
01881      IF ER-FILE-NAME = 'ERPNDM'                                   
01882        AND ER-ACTION = 'LOAD'                                     
01883          OPEN OUTPUT ERPNDM                                       
01884          PERFORM 1000-ERROR-CHECK                                 
01885          MOVE LOW-VALUES         TO PENDING-MAILING-DATA IN ERPNDM
01886          MOVE ALL '9'            TO PM-CONTROL-PRIMARY   IN ERPNDM
01887          WRITE PENDING-MAILING-DATA IN ERPNDM                     
01888          PERFORM 1000-ERROR-CHECK                                 
01889          CLOSE ERPNDM                                             
01890          PERFORM 1000-ERROR-CHECK                                 
01891          DISPLAY 'ERPNDM IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01892                                                                   
01893      IF ER-FILE-NAME = 'ERPNDM'                                   
01894        AND ER-ACTION = 'DELETE'                                   
01895          OPEN I-O ERPNDM                                          
01896          PERFORM 1000-ERROR-CHECK                                 
01897          MOVE ALL '9'        TO PM-CONTROL-PRIMARY IN ERPNDM      
01898          DELETE ERPNDM                                            
01899          PERFORM 1000-ERROR-CHECK                                 
01900          CLOSE ERPNDM                                             
01901          PERFORM 1000-ERROR-CHECK                                 
01902          DISPLAY 'ERPNDM IS UPDATED (UNLESS NOTED ABOVE)'.        
01903                                                                   
01904      IF ER-FILE-NAME = 'ERRQST'                                   
01905        AND ER-ACTION = 'LOAD'                                     
01906          OPEN OUTPUT ERRQST                                       
01907          PERFORM 1000-ERROR-CHECK                                 
01908          MOVE LOW-VALUES          TO AR-REQUEST-RECORD  IN ERRQST 
01909          MOVE ALL '9'             TO RQ-CONTROL-PRIMARY IN ERRQST 
01910          WRITE AR-REQUEST-RECORD IN ERRQST                        
01911          PERFORM 1000-ERROR-CHECK                                 
01912          CLOSE ERRQST                                             
01913          PERFORM 1000-ERROR-CHECK                                 
01914          DISPLAY 'ERRQST IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01915                                                                   
01916      IF ER-FILE-NAME = 'ERRQST'                                   
01917        AND ER-ACTION = 'DELETE'                                   
01918          OPEN I-O ERRQST                                          
01919          PERFORM 1000-ERROR-CHECK                                 
01920          MOVE ALL '9'      TO RQ-CONTROL-PRIMARY IN ERRQST        
01921          DELETE ERRQST                                            
01922          PERFORM 1000-ERROR-CHECK                                 
01923          CLOSE ERRQST                                             
01924          PERFORM 1000-ERROR-CHECK                                 
01925          DISPLAY 'ERRQST IS UPDATED (UNLESS NOTED ABOVE)'.        
01926  EJECT                                                            
01927      IF ER-FILE-NAME = 'ERSUMM'                                   
01928        AND ER-ACTION = 'LOAD'                                     
01929          OPEN OUTPUT ERSUMM                                       
01930          PERFORM 1000-ERROR-CHECK                                 
01931          MOVE LOW-VALUES   TO SUMM-CROSS-REFERENCE IN ERSUMM      
01932          MOVE ALL '9'      TO SX-CONTROL-PRIMARY   IN ERSUMM      
01933          WRITE SUMM-CROSS-REFERENCE IN ERSUMM                     
01934          PERFORM 1000-ERROR-CHECK                                 
01935          CLOSE ERSUMM                                             
01936          PERFORM 1000-ERROR-CHECK                                 
01937          DISPLAY 'ERSUMM IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01938                                                                   
01939      IF ER-FILE-NAME = 'ERSUMM'                                   
01940        AND ER-ACTION = 'DELETE'                                   
01941          OPEN I-O ERSUMM                                          
01942          PERFORM 1000-ERROR-CHECK                                 
01943          MOVE ALL '9'    TO SX-CONTROL-PRIMARY IN ERSUMM          
01944          DELETE ERSUMM                                            
01945          PERFORM 1000-ERROR-CHECK                                 
01946          CLOSE ERSUMM                                             
01947          PERFORM 1000-ERROR-CHECK                                 
01948          DISPLAY 'ERSUMM IS UPDATED (UNLESS NOTED ABOVE)'.        
01949                                                                   
01950      IF ER-FILE-NAME = 'ERRECV'                                   
01951        AND ER-ACTION = 'LOAD'                                     
01952          OPEN OUTPUT ERRECV                                       
01953          PERFORM 1000-ERROR-CHECK                                 
01954          MOVE LOW-VALUES       TO ACCOUNTS-RECEIVABLE IN ERRECV   
01955          MOVE ALL '9'          TO AR-CONTROL-PRIMARY  IN ERRECV   
01956          WRITE ACCOUNTS-RECEIVABLE IN ERRECV                      
01957          PERFORM 1000-ERROR-CHECK                                 
01958          CLOSE ERRECV                                             
01959          PERFORM 1000-ERROR-CHECK                                 
01960          DISPLAY 'ERRECV IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01961                                                                   
01962      IF ER-FILE-NAME = 'ERRECV'                                   
01963        AND ER-ACTION = 'DELETE'                                   
01964          OPEN I-O ERRECV                                          
01965          PERFORM 1000-ERROR-CHECK                                 
01966          MOVE ALL '9'     TO AR-CONTROL-PRIMARY IN ERRECV         
01967          DELETE ERRECV                                            
01968          PERFORM 1000-ERROR-CHECK                                 
01969          CLOSE ERRECV                                             
01970          PERFORM 1000-ERROR-CHECK                                 
01971          DISPLAY 'ERRECV IS UPDATED (UNLESS NOTED ABOVE)'.        
01972  EJECT                                                            
01973      IF ER-FILE-NAME = 'ERCKWK'                                   
01974        AND ER-ACTION = 'LOAD'                                     
01975          OPEN OUTPUT ERCKWK                                       
01976          PERFORM 1000-ERROR-CHECK                                 
01977          MOVE LOW-VALUES     TO CHECK-WORK-RECORDS IN ERCKWK      
01978          MOVE ALL '9'        TO CW-CONTROL-PRIMARY IN ERCKWK      
01979          WRITE CHECK-WORK-RECORDS IN ERCKWK                       
01980          PERFORM 1000-ERROR-CHECK                                 
01981          CLOSE ERCKWK                                             
01982          PERFORM 1000-ERROR-CHECK                                 
01983          DISPLAY 'ERCKWK IS INITIALIZED (UNLESS NOTED ABOVE)'.    
01984                                                                   
01985      IF ER-FILE-NAME = 'ERCKWK'                                   
01986        AND ER-ACTION = 'DELETE'                                   
01987          OPEN I-O ERCKWK                                          
01988          PERFORM 1000-ERROR-CHECK                                 
01989          MOVE ALL '9'      TO CW-CONTROL-PRIMARY IN ERCKWK        
01990          DELETE ERCKWK                                            
01991          PERFORM 1000-ERROR-CHECK                                 
01992          CLOSE ERCKWK                                             
01993          PERFORM 1000-ERROR-CHECK                                 
01994          DISPLAY 'ERCKWK IS UPDATED (UNLESS NOTED ABOVE)'.        
01995                                                                   
01996      IF ER-FILE-NAME = 'ERCMCK'                                   
01997        AND ER-ACTION = 'LOAD'                                     
01998          OPEN OUTPUT ERCMCK                                       
01999          PERFORM 1000-ERROR-CHECK                                 
02000          MOVE LOW-VALUES    TO COMM-CHECK-RECORDS IN ERCMCK       
02001          MOVE ALL '9'       TO CK-CONTROL-PRIMARY IN ERCMCK       
02002          WRITE COMM-CHECK-RECORDS IN ERCMCK                       
02003          PERFORM 1000-ERROR-CHECK                                 
02004          CLOSE ERCMCK                                             
02005          PERFORM 1000-ERROR-CHECK                                 
02006          DISPLAY 'ERCMCK IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02007                                                                   
02008      IF ER-FILE-NAME = 'ERCMCK'                                   
02009        AND ER-ACTION = 'DELETE'                                   
02010          OPEN I-O ERCMCK                                          
02011          PERFORM 1000-ERROR-CHECK                                 
02012          MOVE ALL '9'       TO CK-CONTROL-PRIMARY IN ERCMCK       
02013          DELETE ERCMCK                                            
02014          PERFORM 1000-ERROR-CHECK                                 
02015          CLOSE ERCMCK                                             
02016          PERFORM 1000-ERROR-CHECK                                 
02017          DISPLAY 'ERCMCK IS UPDATED (UNLESS NOTED ABOVE)'.        
02018  EJECT                                                            
02019      IF ER-FILE-NAME = 'ERCMKQ'                                   
02020        AND ER-ACTION = 'LOAD'                                     
02021          OPEN OUTPUT ERCMKQ                                       
02022          PERFORM 1000-ERROR-CHECK                                 
02023          MOVE LOW-VALUES     TO COMMISSION-CHECK-QUE IN ERCMKQ    
02024          MOVE ALL '9'        TO MQ-CONTROL-PRIMARY   IN ERCMKQ    
02025          WRITE COMMISSION-CHECK-QUE IN ERCMKQ                     
02026          PERFORM 1000-ERROR-CHECK                                 
02027          CLOSE ERCMKQ                                             
02028          PERFORM 1000-ERROR-CHECK                                 
02029          DISPLAY 'ERCMKQ IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02030                                                                   
02031      IF ER-FILE-NAME = 'ERCMKQ'                                   
02032        AND ER-ACTION = 'DELETE'                                   
02033          OPEN I-O ERCMKQ                                          
02034          PERFORM 1000-ERROR-CHECK                                 
02035          MOVE ALL '9'      TO MQ-CONTROL-PRIMARY IN ERCMKQ        
02036          DELETE ERCMKQ                                            
02037          PERFORM 1000-ERROR-CHECK                                 
02038          CLOSE ERCMKQ                                             
02039          PERFORM 1000-ERROR-CHECK                                 
02040          DISPLAY 'ERCMKQ IS UPDATED (UNLESS NOTED ABOVE)'.        
02041                                                                   
02042      IF ER-FILE-NAME = 'ERFORM'                                   
02043        AND ER-ACTION = 'LOAD'                                     
02044          OPEN OUTPUT ERFORM                                       
02045          PERFORM 1000-ERROR-CHECK                                 
02046          MOVE LOW-VALUES     TO FORM-MASTER          IN ERFORM    
02047          MOVE ALL '9'        TO FO-CONTROL-PRIMARY   IN ERFORM    
02048          WRITE FORM-MASTER IN ERFORM                              
02049          PERFORM 1000-ERROR-CHECK                                 
02050          CLOSE ERFORM                                             
02051          PERFORM 1000-ERROR-CHECK                                 
02052          DISPLAY 'ERFORM IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02053                                                                   
02054      IF ER-FILE-NAME = 'ERFORM'                                   
02055        AND ER-ACTION = 'DELETE'                                   
02056          OPEN I-O ERFORM                                          
02057          PERFORM 1000-ERROR-CHECK                                 
02058          MOVE ALL '9'      TO FO-CONTROL-PRIMARY IN ERFORM        
02059          DELETE ERFORM                                            
02060          PERFORM 1000-ERROR-CHECK                                 
02061          CLOSE ERFORM                                             
02062          PERFORM 1000-ERROR-CHECK                                 
02063          DISPLAY 'ERFORM IS UPDATED (UNLESS NOTED ABOVE)'.        
02064                                                                   
02065      IF ER-FILE-NAME = 'ERRTRO'                                   
02066        AND ER-ACTION = 'LOAD'                                     
02067          OPEN OUTPUT ERRTRO                                       
02068          PERFORM 1000-ERROR-CHECK                                 
02069          MOVE LOW-VALUES     TO RETRO-MASTER-RECORD  IN ERRTRO    
02070          MOVE ALL '9'        TO RM-CONTROL-PRIMARY   IN ERRTRO    
02071          WRITE RETRO-MASTER-RECORD IN ERRTRO                      
02072          PERFORM 1000-ERROR-CHECK                                 
02073          CLOSE ERRTRO                                             
02074          PERFORM 1000-ERROR-CHECK                                 
02075          DISPLAY 'ERRTRO IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02076                                                                   
02077      IF ER-FILE-NAME = 'ERRTRO'                                   
02078        AND ER-ACTION = 'DELETE'                                   
02079          OPEN I-O ERRTRO                                          
02080          PERFORM 1000-ERROR-CHECK                                 
02081          MOVE ALL '9'      TO RM-CONTROL-PRIMARY IN ERRTRO        
02082          DELETE ERRTRO                                            
02083          PERFORM 1000-ERROR-CHECK                                 
02084          CLOSE ERRTRO                                             
02085          PERFORM 1000-ERROR-CHECK                                 
02086          DISPLAY 'ERRTRO IS UPDATED (UNLESS NOTED ABOVE)'.        
02087                                                                   
02088      IF ER-FILE-NAME = 'ELPEND'                                   
02089        AND ER-ACTION = 'LOAD'                                     
02090          OPEN OUTPUT ELPEND                                       
02091          PERFORM 1000-ERROR-CHECK                                 
02092          MOVE LOW-VALUES     TO PENDING-CLAIM-RECORD IN ELPEND    
02093          MOVE ALL '9'        TO SU-CONTROL-PRIMARY   IN ELPEND    
02094          WRITE PENDING-CLAIM-RECORD IN ELPEND                     
02095          PERFORM 1000-ERROR-CHECK                                 
02096          CLOSE ELPEND                                             
02097          PERFORM 1000-ERROR-CHECK                                 
02098          DISPLAY 'ELPEND IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02099                                                                   
02100      IF ER-FILE-NAME = 'ELPEND'                                   
02101        AND ER-ACTION = 'DELETE'                                   
02102          OPEN I-O ELPEND                                          
02103          PERFORM 1000-ERROR-CHECK                                 
02104          MOVE ALL '9'      TO SU-CONTROL-PRIMARY IN ELPEND        
02105          DELETE ELPEND                                            
02106          PERFORM 1000-ERROR-CHECK                                 
02107          CLOSE ELPEND                                             
02108          PERFORM 1000-ERROR-CHECK                                 
02109          DISPLAY 'ELPEND IS UPDATED (UNLESS NOTED ABOVE)'.        
02110                                                                   
02111      IF ER-FILE-NAME = 'ELPOLF'                                   
02112        AND ER-ACTION = 'LOAD'                                     
02113          OPEN OUTPUT ELPOLF                                       
02114          PERFORM 1000-ERROR-CHECK                                 
02115          MOVE LOW-VALUES     TO POLICY-FORM-MASTER   IN ELPOLF    
02116          MOVE ALL '9'        TO PF-CONTROL-PRIMARY   IN ELPOLF    
02117          WRITE POLICY-FORM-MASTER IN ELPOLF                       
02118          PERFORM 1000-ERROR-CHECK                                 
02119          CLOSE ELPOLF                                             
02120          PERFORM 1000-ERROR-CHECK                                 
02121          DISPLAY 'ELPOLF IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02122                                                                   
02123      IF ER-FILE-NAME = 'ELPOLF'                                   
02124        AND ER-ACTION = 'DELETE'                                   
02125          OPEN I-O ELPOLF                                          
02126          PERFORM 1000-ERROR-CHECK                                 
02127          MOVE ALL '9'      TO PF-CONTROL-PRIMARY IN ELPOLF        
02128          DELETE ELPOLF                                            
02129          PERFORM 1000-ERROR-CHECK                                 
02130          CLOSE ELPOLF                                             
02131          PERFORM 1000-ERROR-CHECK                                 
02132          DISPLAY 'ELPOLF IS UPDATED (UNLESS NOTED ABOVE)'.        
02133                                                                   
02134      IF ER-FILE-NAME = 'ERINMS'                                   
02135        AND ER-ACTION = 'LOAD'                                     
02136          OPEN OUTPUT ERINMS                                       
02137          PERFORM 1000-ERROR-CHECK                                 
02138          MOVE LOW-VALUES     TO INVENTORY-MASTER                  
02139          MOVE ALL '9'        TO IM-CONTROL-PRIMARY                
02140          WRITE INVENTORY-MASTER IN ERINMS                         
02141          PERFORM 1000-ERROR-CHECK                                 
02142          CLOSE ERINMS                                             
02143          PERFORM 1000-ERROR-CHECK                                 
02144          DISPLAY 'ERINMS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02145                                                                   
02146      IF ER-FILE-NAME = 'ERINMS'                                   
02147        AND ER-ACTION = 'DELETE'                                   
02148          OPEN I-O ERINMS                                          
02149          PERFORM 1000-ERROR-CHECK                                 
02150          MOVE ALL '9'      TO IM-CONTROL-PRIMARY IN ERINMS        
02151          DELETE ERINMS                                            
02152          PERFORM 1000-ERROR-CHECK                                 
02153          CLOSE ERINMS                                             
02154          PERFORM 1000-ERROR-CHECK                                 
02155          DISPLAY 'ERINMS UPDATED (UNLESS NOTED ABOVE)'.           
02156                                                                   
02157      IF ER-FILE-NAME = 'ERARBR'                                   
02158        AND ER-ACTION = 'LOAD'                                     
02159          OPEN OUTPUT ERARBR                                       
02160          PERFORM 1000-ERROR-CHECK                                 
02161          MOVE LOW-VALUES     TO ACCOUNTS-RECEIVABLE-BALANCE       
02162          MOVE ALL '9'        TO AB-CONTROL-PRIMARY IN ERARBR      
02163          WRITE ACCOUNTS-RECEIVABLE-BALANCE                        
02164          PERFORM 1000-ERROR-CHECK                                 
02165          CLOSE ERARBR                                             
02166          PERFORM 1000-ERROR-CHECK                                 
02167          DISPLAY 'ERARBR IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02168                                                                   
02169      IF ER-FILE-NAME = 'ERARBR'                                   
02170        AND ER-ACTION = 'DELETE'                                   
02171          OPEN I-O ERARBR                                          
02172          PERFORM 1000-ERROR-CHECK                                 
02173          MOVE ALL '9'      TO AB-CONTROL-PRIMARY IN ERARBR        
02174          DELETE ERARBR                                            
02175          PERFORM 1000-ERROR-CHECK                                 
02176          CLOSE ERARBR                                             
02177          PERFORM 1000-ERROR-CHECK                                 
02178          DISPLAY 'ERARBR UPDATED (UNLESS NOTED ABOVE)'.           
02179                                                                   
02180      IF ER-FILE-NAME = 'ERCCAP'                                   
02181        AND ER-ACTION = 'LOAD'                                     
02182          OPEN OUTPUT ERCCAP                                       
02183          PERFORM 1000-ERROR-CHECK                                 
02184          MOVE LOW-VALUES     TO RESIDENT-STATE-COMMISSION-CAPS    
02185          MOVE ALL '9'        TO ERCCAP-PRIMARY-KEY IN ERCCAP      
02186          WRITE RESIDENT-STATE-COMMISSION-CAPS                     
02187          PERFORM 1000-ERROR-CHECK                                 
02188          CLOSE ERCCAP                                             
02189          PERFORM 1000-ERROR-CHECK                                 
02190          DISPLAY 'ERCCAP IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02191                                                                   
02192      IF ER-FILE-NAME = 'ERCCAP'                                   
02193        AND ER-ACTION = 'DELETE'                                   
02194          OPEN I-O ERCCAP                                          
02195          PERFORM 1000-ERROR-CHECK                                 
02196          MOVE ALL '9'        TO ERCCAP-PRIMARY-KEY IN ERCCAP      
02197          DELETE ERCCAP                                            
02198          PERFORM 1000-ERROR-CHECK                                 
02199          CLOSE ERCCAP                                             
02200          PERFORM 1000-ERROR-CHECK                                 
02201          DISPLAY 'ERCCAP UPDATED (UNLESS NOTED ABOVE)'.           
02202                                                                   
02203      IF ER-FILE-NAME = 'ELNOTE'                                   
02204        AND ER-ACTION = 'LOAD'                                     
02205          OPEN OUTPUT ELNOTE                                       
02206          PERFORM 1000-ERROR-CHECK                                 
02207          MOVE LOW-VALUES     TO CLAIM-EOB-NOTES                   
02208          MOVE ALL '9'        TO EN-CONTROL-PRIMARY IN ELNOTE      
02209          WRITE CLAIM-EOB-NOTES                                    
02210          PERFORM 1000-ERROR-CHECK                                 
02211          CLOSE ELNOTE                                             
02212          PERFORM 1000-ERROR-CHECK                                 
02213          DISPLAY 'ELNOTE IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02214                                                                   
02215      IF ER-FILE-NAME = 'ELNOTE'                                   
02216        AND ER-ACTION = 'DELETE'                                   
02217          OPEN I-O ELNOTE                                          
02218          PERFORM 1000-ERROR-CHECK                                 
02219          MOVE ALL '9'        TO EN-CONTROL-PRIMARY IN ELNOTE      
02220          DELETE ELNOTE                                            
02221          PERFORM 1000-ERROR-CHECK                                 
02222          CLOSE ELNOTE                                             
02223          PERFORM 1000-ERROR-CHECK                                 
02224          DISPLAY 'ELNOTE UPDATED (UNLESS NOTED ABOVE)'.           
02225                                                                   
02226      IF ER-FILE-NAME = 'ERRESS'                                   
02227        AND ER-ACTION = 'LOAD'                                     
02228          OPEN OUTPUT ERRESS                                       
02229          PERFORM 1000-ERROR-CHECK                                 
02230          MOVE LOW-VALUES     TO RESIDENT-STATE-TAX-MASTER         
02231          MOVE ALL '9'        TO ERRESS-PRIMARY-KEY IN ERRESS      
02232          WRITE RESIDENT-STATE-TAX-MASTER                          
02233          PERFORM 1000-ERROR-CHECK                                 
02234          CLOSE ERRESS                                             
02235          PERFORM 1000-ERROR-CHECK                                 
02236          DISPLAY 'ERRESS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02237                                                                   
02238      IF ER-FILE-NAME = 'ERRESS'                                   
02239        AND ER-ACTION = 'DELETE'                                   
02240          OPEN I-O ERRESS                                          
02241          PERFORM 1000-ERROR-CHECK                                 
02242          MOVE ALL '9'        TO ERRESS-PRIMARY-KEY IN ERRESS      
02243          DELETE ERRESS                                            
02244          PERFORM 1000-ERROR-CHECK                                 
02245          CLOSE ERRESS                                             
02246          PERFORM 1000-ERROR-CHECK                                 
02247          DISPLAY 'ERRESS UPDATED (UNLESS NOTED ABOVE)'.           
02248                                                                   
02249      IF ER-FILE-NAME = 'ERRESC'                                   
02250        AND ER-ACTION = 'LOAD'                                     
02251          OPEN OUTPUT ERRESC                                       
02252          PERFORM 1000-ERROR-CHECK                                 
02253          MOVE LOW-VALUES     TO ACCOUNT-RESIDENT-ST-COMMISSION    
02254          MOVE ALL '9'        TO ERRESC-RECORD-KEY IN ERRESC       
02255          WRITE ACCOUNT-RESIDENT-ST-COMMISSION                     
02256          PERFORM 1000-ERROR-CHECK                                 
02257          CLOSE ERRESC                                             
02258          PERFORM 1000-ERROR-CHECK                                 
02259          DISPLAY 'ERRESS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02260                                                                   
02261      IF ER-FILE-NAME = 'ERRESC'                                   
02262        AND ER-ACTION = 'DELETE'                                   
02263          OPEN I-O ERRESC                                          
02264          PERFORM 1000-ERROR-CHECK                                 
02265          MOVE ALL '9'        TO ERRESC-RECORD-KEY IN ERRESC       
02266          DELETE ERRESC                                            
02267          PERFORM 1000-ERROR-CHECK                                 
02268          CLOSE ERRESC                                             
02269          PERFORM 1000-ERROR-CHECK                                 
02270          DISPLAY 'ERRESC UPDATED (UNLESS NOTED ABOVE)'.           
02271                                                                   
02272      IF ER-FILE-NAME = 'ELACHP'                                   
02273        AND ER-ACTION = 'LOAD'                                     
02274          OPEN OUTPUT ELACHP                                       
02275          PERFORM 1000-ERROR-CHECK                                 
02276          MOVE LOW-VALUES     TO ACH-PRENOTIFICATION               
02277          MOVE ALL '9'        TO AP-CONTROL-PRIMARY IN ELACHP      
02278          WRITE ACH-PRENOTIFICATION                                
02279          PERFORM 1000-ERROR-CHECK                                 
02280          CLOSE ELACHP                                             
02281          PERFORM 1000-ERROR-CHECK                                 
02282          DISPLAY 'ELACHP IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02283                                                                   
02284      IF ER-FILE-NAME = 'ELACHP'                                   
02285        AND ER-ACTION = 'DELETE'                                   
02286          OPEN I-O ELACHP                                          
02287          PERFORM 1000-ERROR-CHECK                                 
02288          MOVE ALL '9'        TO AP-CONTROL-PRIMARY IN ELACHP      
02289          DELETE ELACHP                                            
02290          PERFORM 1000-ERROR-CHECK                                 
02291          CLOSE ELACHP                                             
02292          PERFORM 1000-ERROR-CHECK                                 
02293          DISPLAY 'ELACHP UPDATED (UNLESS NOTED ABOVE)'.           
02294                                                                   
02295      IF ER-FILE-NAME = 'ELBANK'                                   
02296        AND ER-ACTION = 'LOAD'                                     
02297          OPEN OUTPUT ELBANK                                       
02298          PERFORM 1000-ERROR-CHECK                                 
02299          MOVE LOW-VALUES     TO BANK-MASTER                       
02300          MOVE ALL '9'        TO BM-CONTROL-PRIMARY IN ELBANK      
02301          WRITE BANK-MASTER                                        
02302          PERFORM 1000-ERROR-CHECK                                 
02303          CLOSE ELBANK                                             
02304          PERFORM 1000-ERROR-CHECK                                 
02305          DISPLAY 'ELBANK IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02306                                                                   
02307      IF ER-FILE-NAME = 'ELBANK'                                   
02308        AND ER-ACTION = 'DELETE'                                   
02309          OPEN I-O ELBANK                                          
02310          PERFORM 1000-ERROR-CHECK                                 
02311          MOVE ALL '9'        TO BM-CONTROL-PRIMARY IN ELBANK      
02312          DELETE ELBANK                                            
02313          PERFORM 1000-ERROR-CHECK                                 
02314          CLOSE ELBANK                                             
02315          PERFORM 1000-ERROR-CHECK                                 
02316          DISPLAY 'ELBANK UPDATED (UNLESS NOTED ABOVE)'.           
02317                                                                   
02318      IF ER-FILE-NAME = 'ERLOSS'                                   
02319        AND ER-ACTION = 'LOAD'                                     
02320          OPEN OUTPUT ERLOSS                                       
02321          PERFORM 1000-ERROR-CHECK                                 
02322          MOVE LOW-VALUES     TO LOSS-RATIO-MASTER                 
02323          MOVE ALL '9'        TO LR-CONTROL IN ERLOSS              
02324          WRITE LOSS-RATIO-MASTER                                  
02325          PERFORM 1000-ERROR-CHECK                                 
02326          CLOSE ERLOSS                                             
02327          PERFORM 1000-ERROR-CHECK                                 
02328          DISPLAY 'ERLOSS IS INITIALIZED (UNLESS NOTED ABOVE)'.    
02329                                                                   
02330      IF ER-FILE-NAME = 'ERLOSS'                                   
02331        AND ER-ACTION = 'DELETE'                                   
02332          OPEN I-O ERLOSS                                          
02333          PERFORM 1000-ERROR-CHECK                                 
02334          MOVE ALL '9'        TO LR-CONTROL IN ERLOSS              
02335          DELETE ERLOSS                                            
02336          PERFORM 1000-ERROR-CHECK                                 
02337          CLOSE ERLOSS                                             
02338          PERFORM 1000-ERROR-CHECK                                 
02339          DISPLAY 'ERLOSS UPDATED (UNLESS NOTED ABOVE)'.           
02340                                                                   
02341      GO TO 0100-READ.                                             
02342                                                                   
02343  1000-ERROR-CHECK.                                                
02344      IF CHK NOT = ZEROS                                           
02345          DISPLAY 'CHECK FIELDS = ', CHK                           
02346          DISPLAY ' I/O ERROR  FILE INIT SKIPPED'.                 
02347                                                                   
02348      MOVE ZEROS         TO CHK.                                   
02349                                                                   
02350  9999-EOJ.                                                        
02351      CLOSE CARD-FILE.                                             
02352                                                                   
02353      GOBACK.                                                      
02354                                                                   
