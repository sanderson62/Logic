00001  IDENTIFICATION DIVISION.                                         03/08/96
00002                                                                   EL127
00003  PROGRAM-ID.                 ELxxx .                                 LV014
00009 *AUTHOR.    pablo.                                                   CL*13
00010 *           colleyville, texas.                                      CL*13
00011                                                                   EL127
00012 *DATE-COMPILED.                                                      CL*13
00024 *REMARKS.                                                            CL**3
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR       CL**3
00026 *    THE CERTIFICATE LOOK-UP.                                        CL**3
00027                                                                   EL127
00057                                                                   EL127
00058      EJECT                                                        EL127
00059  ENVIRONMENT DIVISION.                                            EL127
00060                                                                   EL127
00061  DATA DIVISION.                                                   EL127
00062                                                                   EL127
00063  WORKING-STORAGE SECTION.                                         EL127
00064                                                                   EL127
00065                                                                   EL127
00066  77  FILLER  PIC X(32)  VALUE '********************************'. EL127
00067  77  FILLER  PIC X(32)  VALUE '*    ELxxx WORKING STORAGE     *'. EL127
00068  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.    CL*14
00069                                                                   EL127

       01  dm-record.
           12  dm-control-primary          pic x(09).
           12  dm-rest-of-record           pic x(200).
00074  01  WS-DATE-AREA.                                                EL127
00075      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.   EL127
00076      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.   EL127
00077                                                                   EL127
00078  01  FILLER                          COMP-3.                      EL127
00079      05  WS-READNEXT-SW              PIC x     value ' '.
               88  the-end-of-the-world              value 'Y'.
00083                                                                   EL127
00084  01  FILLER         COMP SYNC.                                       CL**5
00085      05  SC-ITEM                     PIC S9(4)    VALUE +0001.    EL127
00086                                                                   EL127
00087  01  FILLER.                                                      EL127
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           05  ws-browse-sw                pic x     value ' '.
               88  browse-started                    value 'y'.
00088      05  XCTL-725                    PIC X(8)     VALUE 'EL725'.     CL**5
00089      05  QID.                                                     EL127
00090          10  QID-TERM                PIC X(4).                    EL127
00091          10  FILLER                  PIC X(4)     VALUE '127A'.   EL127
00092      05  QID-ITEM                    PIC S9(4)    VALUE +1 COMP.  EL127
00093      05  WS-KEY-LENGTH               PIC S9(4)    VALUE +0 COMP.     CL*13
00094                                                                      CL*13
00095      05  PART-KEY-ON-SW              PIC X(01)    VALUE 'N'.         CL*13
00096          88  PART-KEY-ON                          VALUE 'Y'.         CL*13
00097                                                                      CL*13
00098      05  PART-FIELD-ON-SW            PIC X(01)    VALUE ' '.         CL*13
00099          88  PART-FIELD-ACCT                      VALUE 'A'.         CL*13
00100          88  PART-FIELD-STATE                     VALUE 'S'.         CL*13
00101          88  PART-FIELD-CERT                      VALUE 'C'.         CL*13
00102                                                                   EL127
00103      05  WS-CNTL-KEY.                                             EL127
00104          10  WS-CNTL-ID              PIC X(3).                    EL127
00105          10  WS-CNTL-TYPE            PIC X.                       EL127
00106          10  WS-CNTL-USER            PIC X(4)     VALUE SPACES.   EL127
00107          10  WS-CNTL-SEQ             PIC S9(4)    VALUE +0 COMP.  EL127
00108                                                                   EL127
00109      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL127S'. EL127
00110      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127A'. EL127
00111                                                                   EL127
00112      05  FILLER                      REDEFINES                    EL127
00113          WS-MAP-NAME.                                             EL127
00114          10  FILLER                  PIC XX.                      EL127
00115          10  WS-MAP-NUMBER           PIC X(4).                    EL127
00116          10  FILLER                  PIC XX.                         CL*14
00117                                                                   EL127
00118      05  THIS-PGM                    PIC X(8)     VALUE 'EL127'.  EL127
00119                                                                   EL127
00120      05  WS-CNTL-REC-FOUND-SW        PIC X(01)    VALUE SPACE.       CL**8
00121      05  WS-NEXT-COMPANY-ID          PIC X(03)    VALUE SPACES.      CL**8
00122                                                                      CL**8
00123      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'. EL127
00124      05  WS-ACCOUNT-MASTER-DSID      PIC X(8)     VALUE 'ERACCT2'.EL127
00125      05  WS-CERT-MASTER-DSID         PIC X(8)     VALUE 'ELCERT'. EL127
00126      05  WS-CERT-AIX01-DSID          PIC X(8)     VALUE 'ELCERT2'.EL127
00127      05  WS-CERT-AIX02-DSID          PIC X(8)     VALUE 'ELCERT3'.EL127
00128      05  WS-CERT-AIX03-DSID          PIC X(8)     VALUE 'ELCERT4'.EL127
00129      05  WS-CERT-AIX04-DSID          PIC X(8)     VALUE 'ELCERT5'.EL127
00130      05  WS-CERT-AIX05-DSID          PIC X(8)     VALUE 'ELCERT6'.EL127
00131                                                                   EL127
00132      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX1'.   EL127
00133                                                                      CL*13
00134      05  WK-SC-STATE.                                                CL*13
00135          12  WK-SC-STATE-1           PIC X.                          CL*13
00136          12  WK-SC-STATE-2           PIC X.                          CL*13
00137                                                                      CL*13
00138      05  WK-SC-CERT.                                                 CL*13
00139          12  WK-SC-CERT-1            PIC X.                          CL*13
00140          12  WK-SC-CERT-2            PIC X.                          CL*13
00141          12  WK-SC-CERT-3            PIC X.                          CL*13
00142          12  WK-SC-CERT-4            PIC X.                          CL*13
00143          12  WK-SC-CERT-5            PIC X.                          CL*13
00144          12  WK-SC-CERT-6            PIC X.                          CL*13
00145          12  WK-SC-CERT-7            PIC X.                          CL*13
00146          12  WK-SC-CERT-8            PIC X.                          CL*13
00147          12  WK-SC-CERT-9            PIC X.                          CL*13
00148          12  WK-SC-CERT-10           PIC X.                          CL*13
00149                                                                   EL127
00150      05  WS-DEEDIT-FIELD             PIC X(15)    VALUE ZERO.     EL127
00151                                                                   EL127
00152      05  WS-DEEDIT-FIELD-V0          REDEFINES                    EL127
00153          WS-DEEDIT-FIELD             PIC S9(15).                  EL127
00154                                                                   EL127
00155      05  WS-INPUT-FIELD              PIC X(50)    VALUE SPACES.   EL127
00156                                                                   EL127
00157      05  WS-INPUT-CHAR               REDEFINES                    EL127
00158          WS-INPUT-FIELD              PIC X                        EL127
00159          OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.      EL127
00160                                                                   EL127
00161  01  WS-FIRST-NAME.                                                  CL*10
00162      05  WS-FIRST-INITIAL            PIC X        VALUE SPACES.      CL**9
00163      05  WS-FIRST-REST               PIC X(14)    VALUE SPACES.      CL**9
00164                                                                      CL**9
00165  01  WS-INITIALS.                                                    CL**9
00166      05  WS-INITIAL-FIRST            PIC X        VALUE SPACES.      CL**9
00167      05  WS-INITIAL-MIDDLE           PIC X        VALUE SPACES.      CL**9
00168                                                                      CL**9
00169      05  PI-ACCOUNT-KEY.                                          EL127
00170          10  PI-AK-COMPANY-CD        PIC X.                       EL127
00171          10  PI-AK-CARRIER           PIC X.                       EL127
00172          10  PI-AK-GROUP             PIC X(06).                      CL**6
00173          10  PI-AK-STATE             PIC XX.                      EL127
00174          10  PI-AK-ACCOUNT           PIC X(10).                      CL**6
00175          10  PI-AK-EXPIRE-DATE       PIC XX.                      EL127
00176                                                                   EL127
00177      EJECT                                                        EL127
00178      05  ERROR-MESSAGES.                                          EL127
00179          10  ER-0004                 PIC X(4)     VALUE '0004'.   EL127
00180          10  ER-0008                 PIC X(4)     VALUE '0008'.   EL127
00181          10  ER-0019                 PIC X(4)     VALUE '0019'.      CL**8
00182          10  ER-0022                 PIC X(4)     VALUE '0022'.   EL127
00183          10  ER-0029                 PIC X(4)     VALUE '0029'.   EL127
00184          10  ER-0070                 PIC X(4)     VALUE '0070'.   EL127
00185          10  ER-0089                 PIC X(4)     VALUE '0089'.      CL**8
00186          10  ER-0194                 PIC X(4)     VALUE '0194'.   EL127
00187          10  ER-0195                 PIC X(4)     VALUE '0195'.   EL127
00188          10  ER-0196                 PIC X(4)     VALUE '0196'.   EL127
00189          10  ER-0197                 PIC X(4)     VALUE '0197'.   EL127
00190          10  ER-0198                 PIC X(4)     VALUE '0198'.   EL127
00191          10  ER-0201                 PIC X(4)     VALUE '0201'.   EL127
00192          10  ER-0210                 PIC X(4)     VALUE '0210'.   EL127
00193          10  ER-0215                 PIC X(4)     VALUE '0215'.   EL127
00194          10  ER-0216                 PIC X(4)     VALUE '0216'.   EL127
00195          10  ER-0228                 PIC X(4)     VALUE '0228'.      CL**8
00196          10  ER-0488                 PIC X(4)     VALUE '0488'.   EL127
00197          10  ER-0671                 PIC X(4)     VALUE '0671'.      CL**4
00198          10  ER-0764                 PIC X(4)     VALUE '0764'.      CL**9
00199          10  ER-0765                 PIC X(4)     VALUE '0765'.      CL*10
00200          10  ER-2370                 PIC X(4)     VALUE '2370'.   EL127
00201          10  ER-2371                 PIC X(4)     VALUE '2371'.   EL127
00202          10  ER-2373                 PIC X(4)     VALUE '2373'.   EL127
00203          10  ER-8100                 PIC X(4)     VALUE '8100'.      CL*13
00204          10  ER-8101                 PIC X(4)     VALUE '8101'.      CL*13
00205          10  ER-8102                 PIC X(4)     VALUE '8102'.      CL*13
00206          10  ER-8103                 PIC X(4)     VALUE '8103'.      CL*13
00207          10  ER-8104                 PIC X(4)     VALUE '8104'.      CL*13
00208          10  ER-8105                 PIC X(4)     VALUE '8105'.      CL*13
00209          10  ER-8106                 PIC X(4)     VALUE '8106'.      CL*13
00210          10  ER-8107                 PIC X(4)     VALUE '8107'.      CL*13
00211                                                                   EL127
00220                                                                      CL*13
00221                                                                   EL127

00226                                      COPY ELCDATE.                   CL**8
00227                                                                   EL127
00239                                                                   EL127
00246                                                                   EL127
00247      EJECT                                                        EL127
00248  LINKAGE SECTION.                                                 EL127
00249  01  DFHCOMMAREA                     PIC X(1024).                 EL127
00250                                                                   EL127
00266                                                                   EL127
00267      EJECT                                                        EL127
00268  PROCEDURE DIVISION.                                              EL127
00269                                                                   EL127
00270      CONTINUE.                                                       CL*13
00271                                                                   EL127
00275      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE                    EL127
00276      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE                EL127
00277      perform 0015-startbr-erdump thru 0015-exit
           perform 0020-read-next      thru 0020-exit
           perform 0030-process        thru 0030-exit until
               the-end-of-the-world

00328      .                                                             EL127
00329  0015-startbr-erdump.

00812      move zeros         to Dm-control-PRimary.                    EL127
00813      EXEC CICS STARTBR                                            EL127
00814          DATASET   ('erdump')                                     EL127
00815          RIDFLD    (dm-control-primary)                           EL127
00816          GTEQ                                                     EL127
               resp      (ws-response)
00817      END-EXEC

           if resp-normal
              set browse-started to true
           end-if

           .
       0015-exit.
           exit.


       0020-read-next.                                                  EL127

00822      EXEC CICS READNEXT                                           EL127
00823          DATASET   ('erdump')                                     EL127
00824          RIDFLD    (dm-control-primary)                           EL127
00825          into      (dm-record)
               resp      (ws-response)
00826      END-EXEC.                                                    EL127
00827
       0020-exit.
           exit.

       0030-process.

           if resp-normal
00350         move dm-rest-of-record   to date-conversion-data
              perform 8500-date-conversion
           else
              if browse-started
                 move ' ' to ws-browse-sw
                 perform 0040-end-browse thru 0040-exit
                 move zeros  to dm-control-primary
pemtst           perform 0015-startbr-erdump thru 0015-exit
              end-if
           end-if

00352                                                                   EL127
00353      PERFORM 0020-READ-NEXT thru 0020-exit

00354      .                                                            EL127
01737  0030-exit.
           exit.

00329  0040-end-browse.

00813      EXEC CICS endbr
00814          DATASET   ('erdump')                                     EL127
00817      END-EXEC

           .
       0040-exit.
           exit.

01738      EJECT                                                        EL127
01754  8500-DATE-CONVERSION SECTION.                                    EL127
01755      EXEC CICS LINK                                               EL127
01756          PROGRAM  ('ELDATCV')                                     EL127
01757          COMMAREA (DATE-CONVERSION-DATA)                          EL127
01758          LENGTH   (DC-COMM-LENGTH)                                EL127
01759      END-EXEC.                                                    EL127
01760                                                                   EL127
01761  8500-EXIT.                                                       EL127
01762      EXIT.                                                        EL127
01763                                                                   EL127
01773      EJECT                                                        EL127
01864  9999-LAST-PARAGRAPH SECTION.                                     EL127
01865      GOBACK.                                                      EL127
