       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID.               CICLEXT.                               00000011
      *AUTHOR.   DAVE WOLATZ.                                           00000030
      *DATE-COMPILED.                                                   00000040
      *REMARKS.                                                         00000050
      ***************************************************************** 00000051
      *      D O   N O T   U S E
      *     T H I S   I S   U S E D   T O   R E C R E A T E   C L A I M
      *     R E C O R D S    F O R    G / L   I N T E R F A C E


      * CHANGED AND COMPILED AS COB2.                                   00000051
      *                                                                 00000051
      *                                                                 00000051
LGC003***************************************************************** 00000051
LGC003*****    THIS PROGRAM IS A COMPLETE COPY OF DMCLEXT.        ***** 00000052
LGC003*****    THE PROGRAM WAS COPIED FOR THE LOGIC REINSTALL     ***** 00000053
LGC003*****    ON JUNE 6, 1988 AND WAS MODIFIED EXTENSIVLY TO     ***** 00000054
LGC003*****    PROCESS CLAS-IC II LOGIC.                          ***** 00000055
LGC003*****    COPYBOOKS WERE NOT USED DUE TO THE NEED TO HAVE    ***** 00000056
LGC003*****    SEVERAL RECORD FORMATS AVAILABLE AT THE SAME TIME. ***** 00000057
LGC003***************************************************************** 00000058
      *THIS PROGRAM PROCESSES ONLY CID RECORDS AND RUNS EACH NIGHT      00000059
      *    AFTER THE SHUTDOWN OF CICS.                                  00000060
      *IT GENERATES THE F-EXTRACT RECORDS FOR PROCESSING BY PROGRAM     00000061
      *    'LGCIFCE'.                                                   00000062
      *THE FILE WHICH TRIGGERS THE CREATION OF THE F-EXTRACT RECORDS IS 00000063
      *  'DLYACTV', WHICH IS BUILT ONLINE DURING THE DAY. PROGRAM       00000064
      *  'EL310' WRITES TO THE 'DLYACTV' FILE ALSO.                     00000065
LGC003*  'EL310' ALSO BUILDS RECORDS FOR AUTOMATIC PAYMENTS AND CLOSES. 00000066
      *  AFTER THE 'DLYACTV' FILE HAS BEEN READ THE RECORDS ARE DELETED 00000067
      *  FROM THE FILE, PROVIDING FOR A "CLEAN" FILE AT THE END OF      00000068
      *  THE EXECUTION OF THIS JOB. (NOTE: THE FIRST RECORD ON THE      00000069
      *  'DLYACTV' FILE IS A DUMMY RECORD, WHICH IS BYPASSED IN THE     00000070
      *  PROCESSING AND NOT DELETED, ALLOWING CICS TO USE THE FILE.     00000071
      *                                                                 00000180
LGC003*    INPUT FILES - ELMSTR  - CLAIM MASTER                         00000181
LGC003*                  ELCERT  - CERTIFICIATE MASTER                  00000182
LGC003*                  ELTRLR  - ACTIVITY TRAILERS                    00000183
LGC003*                  ERACCT  - ACCOUNT MASTER                       00000184
      *    I-O FILE    - DLYACTV - DAILY ACTIVITY FILE ("TICKLER" FILE) 00000230
LGC003*   OUTPUT FILES- CIEXTRS - FA, FB, & FC EXTRACTS                 00000231
      *                 PRNTR   - REPORT                                00000250
      *                                                                 00000260
       ENVIRONMENT DIVISION.                                            00000270
       INPUT-OUTPUT SECTION.                                            00000280
       FILE-CONTROL.                                                    00000290
           SELECT DLYACTV          ASSIGN TO SYS011-DLYACTV             00000300
                                   ORGANIZATION IS INDEXED              00000310
                                   ACCESS IS SEQUENTIAL                 00000320
                                   RECORD KEY IS DA-KEY                 00000330
                                   FILE STATUS IS DLYACTV-FILE-STATUS.  00000340
                                                                        00000350
LGC003     SELECT ELMSTR           ASSIGN TO SYS012-FBA1-ELMSTR         00000351
                                   ORGANIZATION IS INDEXED              00000370
                                   ACCESS IS DYNAMIC                    00000380
                                   RECORD KEY IS CL-CONTROL-PRIMARY     00000390
LGC003                             FILE STATUS IS ELMSTR-FILE-STATUS.   00000391
                                                                        00000410
LGC003     SELECT ELCERT           ASSIGN TO SYS013-FBA1-ELCERT         00000411
                                   ORGANIZATION IS INDEXED              00000430
                                   ACCESS IS DYNAMIC                    00000440
                                   RECORD KEY IS CM-CONTROL-PRIMARY     00000450
LGC003                             FILE STATUS IS ELCERT-FILE-STATUS.   00000451
                                                                        00000470
LGC003     SELECT ELTRLR           ASSIGN TO SYS014-FBA1-ELTRLR         00000471
                                   ORGANIZATION IS INDEXED              00000490
                                   ACCESS IS DYNAMIC                    00000500
                                   RECORD KEY IS AT-CONTROL-PRIMARY     00000510
LGC003                             FILE STATUS IS ELTRLR-FILE-STATUS.   00000511
                                                                        00000530
LGC003     SELECT ERACCT           ASSIGN TO SYS015-FBA1-ERACCT         00000531
                                   ORGANIZATION IS INDEXED              00000550
                                   ACCESS IS DYNAMIC                    00000560
                                   RECORD KEY IS AM-CONTROL-PRIMARY     00000570
LGC003                             FILE STATUS IS ERACCT-FILE-STATUS.   00000571
                                                                        00000590
LGC003     SELECT CIEXTRS          ASSIGN TO SYS010-UT-FBA1-S-CIEXTRS.  00000591
                                                                        00000610
           SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   00000620
                                                                        00000630
       DATA DIVISION.                                                   00000640
       FILE SECTION.                                                    00000650
       FD  DLYACTV.                                                     00000660
LGC003               COPY ELCDAR.                                       00000681
                                                                        00000700
           EJECT                                                        00000710
LGC003 FD  ELMSTR.                                                      00000711
LGC003               COPY ELCMSTR.                                      00000741
                                                                        00000760
           EJECT                                                        00000770
LGC003 FD  ELCERT.                                                      00000771
LGC003               COPY ELCCERT.                                      00000801
                                                                        00000820
           EJECT                                                        00000830
LGC003 FD  ELTRLR.                                                      00000831
LGC003               COPY ELCTRLR.                                      00000861
                                                                        00000880
           EJECT                                                        00000890
LGC003 FD  ERACCT.                                                      00000891
LGC003               COPY ERCACCT.                                      00000921
                                                                        00000940
           EJECT                                                        00000950
LGC003 FD  CIEXTRS                                                      00000951
           LABEL RECORDS ARE OMITTED                                    00001040
           RECORDING MODE IS F.                                         00001040
LGC003 01  EXTRACT-FILE-RECORD     PICTURE X(314).                      00001001
                                                                        00001020
       FD  PRNTR                                                        00001030
           LABEL RECORDS ARE OMITTED                                    00001040
           RECORDING MODE IS F.                                         00001040
       01  PRINT-RECORD            PIC X(132).                          00001080
                                                                        00001090
           EJECT                                                        00001100
       WORKING-STORAGE SECTION.                                         00001110
       01  FILLER  PIC X(32)   VALUE '********************************'.00001120
LGC003 01  FILLER  PIC X(32)   VALUE '*    CICLEXT WORKING STORAGE   *'.00001121
       01  FILLER  PIC X(32)   VALUE '********************************'.00001140
       01  FILLER                          COMPUTATIONAL-3.             00001150
           05  WS-FA-COUNT                 PICTURE S9(7)   VALUE ZERO.  00001160
           05  WS-FB-COUNT                 PICTURE S9(7)   VALUE ZERO.  00001170
           05  WS-FC-COUNT                 PICTURE S9(7)   VALUE ZERO.  00001180
           05  WS-CURRENT-TRLRS            PICTURE S9(5)   VALUE ZERO.  00001190
           05  EXTRACTS-OUT                PICTURE S9(7)   VALUE ZERO.  00001200
           05  RECORDS-IN                  PICTURE S9(7)   VALUE ZERO.  00001210
           05  PAGE-COUNT                  PICTURE S9(3)   VALUE +1.    00001220
           05  LINES-PRINTED               PICTURE S9(3)   VALUE +99.   00001230
           05  LINES-ON-PAGE               PICTURE S9(3)   VALUE +55.   00001240
                                                                        00001250
       01  FILLER.                                                      00001260
           05  DLYACTV-FILE-STATUS         PICTURE XX      VALUE ZERO.  00001270
LGC003     05  ELCERT-FILE-STATUS          PICTURE XX      VALUE ZERO.  00001271
LGC003     05  ELMSTR-FILE-STATUS          PICTURE XX      VALUE ZERO.  00001272
LGC003     05  ELTRLR-FILE-STATUS          PICTURE XX      VALUE ZERO.  00001273
LGC003     05  ERACCT-FILE-STATUS          PICTURE XX      VALUE ZERO.  00001274
           05  DLYACTV-EOF-SW              PIC X           VALUE 'N'.   00001320
               88  DLYACTV-EOF                             VALUE 'Y'.   00001330
DAN        05  DLYACTV-DELETE-SW           PIC X           VALUE 'N'.   00001320
DAN            88  DELETE-DLYACTV                          VALUE 'Y'.   00001330
           05  WS-CLAIM-FOUND-SW           PIC X           VALUE 'N'.   00001340
               88  CLAIM-FOUND                             VALUE 'Y'.   00001350
               88  CLAIM-NOT-FOUND                         VALUE 'N'.   00001360
           05  WS-ACCOUNT-FOUND-SW         PIC X           VALUE 'N'.   00001370
               88  ACCOUNT-FOUND                           VALUE 'Y'.   00001380
LGC003     05  WS-HOLD-KEY                 PIC X(25).                   00001381
           05  WS-CHECK-TRLR-KEY.                                       00001400
               10  WS-CHECK-KEY            PIC X(20).                   00001401
               10  FILLER                  PIC XX.                      00001420
           05  WS-RUN-DATE                 PICTURE X(10).               00001430
           05  WS-COMPANY-ID               PICTURE X(3).                00001440
           05  WS-COMPANY-CD               PICTURE X.                   00001450
           05  WS-ERROR-MESSAGE.                                        00001460
               10  WS-ERROR-PART1          PIC X(23).                   00001470
               10  WS-ERROR-PART2          PIC X(8).                    00001480
               10  FILLER                  PIC X(8)   VALUE ' STATUS='. 00001490
               10  WS-ERROR-STATUS         PIC XX.                      00001500
           05  WS-ZERO.                                                 00001510
               10  WS-ZERO-P1              PIC X.                       00001520
               10  WS-ZERO-P2              PIC X.                       00001530
           05  WS-ZERO-R REDEFINES WS-ZERO  PIC S9(4) COMP.             00001540
           05  WS-FOUR.                                                 00001550
               10  WS-FOUR-P1              PIC X.                       00001560
               10  WS-FOUR-P2              PIC X.                       00001570
           05  WS-FOUR-R REDEFINES WS-FOUR  PIC S9(4) COMP.             00001580
           05  WS-FIVE.                                                 00001590
               10  WS-FIVE-P1              PIC X.                       00001600
               10  WS-FIVE-P2              PIC X.                       00001610
           05  WS-FIVE-R REDEFINES WS-FIVE  PIC S9(4) COMP.             00001620
           05  FORCE-DUMP-X       PIC X.                                00001630
           05  FORCE-DUMP   REDEFINES FORCE-DUMP-X   PIC S9.            00001640
                                                                        00001650
LGC003                                                                  00001651
LGC003     COPY ELCNWA.                                                 00001652
LGC003                                                                  00001653
LGC003 01  REPORTS-EXTRACT-RECORD.                                      00001654
LGC003     12  EX-RECORD-ID                        PIC XX.              00001655
LGC003         88  VALID-EX-ID                        VALUE 'EX'.       00001656
LGC003                                                                  00001657
LGC003     12  EX-SORT-KEY-AREAS.                                       00001658
LGC003         16  EX-POSITIONING-CODE             PIC X.               00001659
LGC003             88  FRONT-OF-FILE                  VALUE '0'.        00001660
LGC003             88  BACK-OF-FILE                   VALUE '9'.        00001661
LGC003         16  EX-EXTRACT-CODE                 PIC X.               00001662
LGC003         16  EX-COMPANY-CD                   PIC X.               00001663
LGC003         16  EX-COMPANY-ID                   PIC XXX.             00001664
LGC003         16  EX-RECORD-TYPE                  PIC X.               00001665
LGC003                                                                  00001666
LGC003         16  EX-SORT-KEY-G.                                       00001667
LGC003             20  EX-SG-CARRIER               PIC X.               00001668
LGC003             20  EX-SG-STATE                 PIC XX.              00001669
LGC003             20  EX-SG-ACCOUNT-NO            PIC X(10).           00001670
LGC003             20  EX-SG-CLAIM-NO              PIC X(7).            00001671
LGC003             20  EX-SG-CERT-NO.                                   00001672
LGC003                 24  EX-SG-CERT-PRIME        PIC X(10).           00001673
LGC003                 24  EX-SG-CERT-SFX          PIC X.               00001674
LGC003             20  EX-SG-TRAILER-SEQ-NO        PIC S9(4)    COMP.   00001675
LGC003                                                                  00001676
LGC003     12  EX-DATA-AREAS                       PIC X(272).          00001677
LGC003                                                                  00001678
00567      12  EX-EXTRACT-F-RECORD-A  REDEFINES  EX-DATA-AREAS.         00001679
00568          16  EX-FA-INSURED-NAME              PIC X(30).           00001680
00569          16  EX-FA-ACCOUNT-NAME              PIC X(30).           00001681
00570          16  EX-FA-INSURED-BIRTH-DT          PIC XX.              00001682
00571          16  EX-FA-INSURED-SEX-CD            PIC X.               00001683
00572          16  EX-FA-INSURED-OCC-CD            PIC XX.              00001684
00573          16  EX-FA-SOC-SEC-NO                PIC X(11).           00001685
00574          16  EX-FA-PROCESSOR-ID              PIC X(4).            00001686
00575                                                                   00001687
00576          16  EX-FA-PROCESSING-INFO.                               00001688
00577              20  EX-FA-CLAIM-STATUS         PIC X.                00001689
00578                  88  EX-FA-CLAIM-IS-OPEN          VALUE 'O'.      00001690
00579                  88  EX-FA-CLAIM-IS-CLOSED        VALUE 'C'.      00001691
00580              20  EX-FA-CLAIM-TYPE           PIC X.                00001692
00581                  88  EX-FA-AH-CLAIM               VALUE 'A'.      00001693
00582                  88  EX-FA-LIFE-CLAIM             VALUE 'L'.      00001694
00583              20  EX-FA-CLAIM-PREM-TYPE      PIC X.                00001695
00584                  88  EX-FA-SINGLE-PREMIUM         VALUE '1'.      00001696
00585                  88  EX-FA-O-B-COVERAGE           VALUE '2'.      00001697
00586                  88  EX-FA-OPEN-END-COVERAGE      VALUE '3'.      00001698
00587              20  EX-FA-INCURRED-DT          PIC XX.               00001699
00588              20  EX-FA-REPORTED-DT          PIC XX.               00001700
00589              20  EX-FA-FILE-ESTABLISH-DT    PIC XX.               00001701
00590              20  EX-FA-EST-END-OF-DISAB-DT  PIC XX.               00001702
00591              20  EX-FA-LAST-PMT-DT          PIC XX.               00001703
00592              20  EX-FA-LAST-PMT-AMT         PIC S9(7)V99  COMP-3. 00001704
00593              20  EX-FA-PAID-THRU-DT         PIC XX.               00001705
00594              20  EX-FA-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3. 00001706
00595              20  EX-FA-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3. 00001707
00596              20  EX-FA-NO-OF-DAYS-PAID      PIC S9(4)     COMP.   00001708
00597              20  EX-FA-PMT-CALC-METHOD      PIC X.                00001709
00598                  88  EX-FA-360-DAY-YR          VALUE '1'.         00001710
00599                  88  EX-FA-365-DAY-YR          VALUE '2'.         00001711
00600                  88  EX-FA-FULL-MONTHS         VALUE '3'.         00001712
00601              20  EX-FA-CAUSE-CD             PIC X(6).             00001713
00602              20  EX-FA-DIAGNOSIS-DESCRIP    PIC X(26).            00001714
00603              20  EX-FA-LAST-REOPEN-DT       PIC XX.               00001715
00604              20  EX-FA-LAST-CLOSE-DT        PIC XX.               00001716
00605              20  EX-FA-LAST-CLOSE-REASON    PIC X.                00001717
00606                  88  EX-FA-FINAL-PAID             VALUE '1'.      00001718
00607                  88  EX-FA-CLAIM-DENIED           VALUE '2'.      00001719
00608                  88  EX-FA-AUTO-CLOSE             VALUE '3'.      00001720
00609                                                                   00001721
00610          16  EX-FA-CERTIFICATE-DATA.                              00001722
00611              20  EX-FA-CERT-ORIGIN          PIC X.                00001723
00612                  88  EX-FA-CERT-WAS-ONLINE        VALUE '1'.      00001724
00613                  88  EX-FA-CERT-WAS-CREATED       VALUE '2'.      00001725
00614                  88  EX-FA-COVERAGE-WAS-ADDED     VALUE '3'.      00001726
00615                                                                   00001727
00616          16  EX-FA-STATUS-CONTROLS.                               00001728
00617              20  EX-FA-PRIORITY-CD          PIC X.                00001729
00618                  88  EX-FA-HIGHEST-PRIORITY       VALUE '9'.      00001730
00619              20  EX-FA-SUPV-ATTN-CD         PIC X.                00001731
00620                  88  EX-FA-SUPV-NOT-REQUIRED      VALUE ' ' 'N'.  00001732
00621                  88  EX-FA-SUPV-IS-REQUIRED       VALUE 'Y'.      00001733
00622              20  EX-FA-PURGED-DT            PIC XX.               00001734
00623              20  EX-FA-RESTORED-DT          PIC XX.               00001735
00624              20  EX-FA-NEXT-AUTO-PAY-DT     PIC XX.               00001736
00625              20  EX-FA-NEXT-RESEND-DT       PIC XX.               00001737
00626              20  EX-FA-NEXT-FOLLOWUP-DT     PIC XX.               00001738
00627              20  FILLER                     PIC XX.               00001739
00628              20  EX-FA-LAST-MAINT-DT        PIC XX.               00001740
00629              20  EX-FA-LAST-MAINT-USER      PIC X(4).             00001741
00630              20  EX-FA-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3. 00001742
00631              20  EX-FA-LAST-MAINT-TYPE      PIC X.                00001743
00632                  88  EX-FA-CLAIM-SET-UP           VALUE ' '.      00001744
00633                  88  EX-FA-PAYMENT-MADE           VALUE '1'.      00001745
00634                  88  EX-FA-LETTER-SENT            VALUE '2'.      00001746
00635                  88  EX-FA-MASTER-WAS-ALTERED     VALUE '3'.      00001747
00636                  88  EX-FA-MASTER-WAS-RESTORED    VALUE '4'.      00001748
00637                  88  EX-FA-INCURRED-DATE-CHANGED  VALUE '5'.      00001749
00638                  88  EX-FA-FILE-CONVERTED         VALUE '6'.      00001750
00639              20  EX-FA-RELATED-CLAIM-NO     PIC X(7).             00001751
00640              20  EX-FA-HISTORY-ARCHIVE-DT   PIC XX.               00001752
00641                                                                   00001753
00642          16  EX-FA-TRAILER-CONTROLS.                              00001754
00643              20  EX-FA-TRAILER-SEQ-CNT      PIC S9(4)     COMP.   00001755
00644                  88  EX-FA-1ST-TRL-AVAIL       VALUE +4095.       00001756
00645                  88  EX-FA-LAST-TRL-AVAIL      VALUE +1.          00001757
00646                  88  EX-FA-RESV-EXP-HIST-TRLR  VALUE +0.          00001758
00647              20  EX-FA-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.   00001759
00648              20  FILLER                     PIC XX.               00001760
00649              20  EX-FA-AUTO-PAY-SEQ         PIC S9(4)     COMP.   00001761
00650              20  EX-FA-ACCOUNT-ADDR-CNT PIC S9(01).               00001762
00651                  88  EX-FA-ACCOUNT-IS-ONLINE  VALUE ZERO.         00001763
00648              20  FILLER                     PIC  X.               00001764
00652                                                                   00001765
00653          16  EX-FA-FILE-LOCATION            PIC X(4).             00001766
00654          16  EX-FA-BENEFICIARY              PIC X(10).            00001767
00655                                                                   00001768
00656          16  FILLER                         PIC X(66).            00001769
00657                                                                   00001770
00658      12  EX-EXTRACT-F-RECORD-B  REDEFINES  EX-DATA-AREAS.         00001771
LGC003         16  EX-FB-INSURED-NAME              PIC X(30).           00001772
LGC003         16  EX-FB-EFFECTIVE-DT              PIC X(02).           00001773
00661          16  EX-FB-SSN                       PIC X(11).           00001774
00662          16  EX-FB-MEMBER-NO                 PIC X(12).           00001775
00663          16  EX-FB-INSURED-ISSUE-AGE         PIC 99.              00001776
00664          16  EX-FB-SPOUSE-ISSUE-AGE          PIC 99.              00001777
00665          16  EX-FB-INSURED-SEX               PIC X.               00001778
00666                                                                   00001779
00667          16  EX-FB-LIFE-DATA.                                     00001780
00668              20  EX-FB-LF-BENEFIT-CD        PIC XX.               00001781
00669              20  EX-FB-LF-ORIG-TERM         PIC S9(3)     COMP-3. 00001782
00670              20  EX-FB-LF-BENEFIT-AMT       PIC S9(9)V99  COMP-3. 00001783
00671              20  EX-FB-LF-PREMIUM-AMT       PIC S9(7)V99  COMP-3. 00001784
00672              20  EX-FB-LF-REMAINING-AMT     PIC S9(9)V99  COMP-3. 00001785
00673              20  EX-FB-LF-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3. 00001786
00674              20  EX-FB-LF-ITD-DEATH-AMT     PIC S9(7)V99  COMP-3. 00001787
LGC003*            20  EX-FB-LF-FORM              PIC X(12).            00001788
00675              20  EX-FB-LF-DEV-CODE          PIC X(3).             00001789
00676                                                                   00001790
00677          16  EX-FB-AH-DATA.                                       00001791
00678              20  EX-FB-AH-BENEFIT-CD        PIC XX.               00001792
00679              20  EX-FB-AH-ORIG-TERM         PIC S9(3)     COMP-3. 00001793
00680              20  EX-FB-AH-BENEFIT-AMT       PIC S9(9)V99  COMP-3. 00001794
00681              20  EX-FB-AH-PREMIUM-AMT       PIC S9(7)V99  COMP-3. 00001795
00682              20  EX-FB-AH-ITD-CANCEL-AMT    PIC S9(5)V99  COMP-3. 00001796
00683              20  EX-FB-AH-ITD-LUMP-PMT      PIC S9(7)V99  COMP-3. 00001797
LGC003*            20  EX-FB-AH-FORM              PIC X(12).            00001798
00684              20  EX-FB-AH-DEV-CODE          PIC X(3).             00001799
00685                                                                   00001800
00686          16  EX-FB-LOAN-INFORMATION.                              00001801
00687              20  EX-FB-LOAN-APR             PIC S999V9999 COMP-3. 00001802
00688              20  EX-FB-PAY-FREQUENCY        PIC S99.              00001803
00689              20  EX-FB-LOAN-TERM            PIC S999      COMP-3. 00001804
00690              20  EX-FB-RATE-CLASS           PIC XX.               00001805
LGC003*            20  EX-FB-RATE-DEV             PIC XXX.              00001806
00691              20  EX-FB-POLICY-FORM-NO       PIC X(12).            00001807
00692                                                                   00001808
00693              20  EX-FB-PREMIUM-TYPE         PIC X.                00001809
00694                  88  EX-FB-SING-PRM            VALUE '1'.         00001810
00695                  88  EX-FB-O-B-COVERAGE        VALUE '2'.         00001811
00696                  88  EX-FB-OPEN-END            VALUE '3'.         00001812
00697              20  EX-FB-IND-GRP-TYPE         PIC X.                00001813
00698                  88  EX-FB-INDIVIDUAL          VALUE 'I'.         00001814
00699                  88  EX-FB-GROUP               VALUE 'G'.         00001815
00700              20  EX-FB-SKIP-CODE            PIC X.                00001816
00701                  88  NO-MONTHS-SKIPPED      VALUE SPACE.          00001817
00702                  88  SKIP-JULY              VALUE '1'.            00001818
00703                  88  SKIP-AUGUST            VALUE '2'.            00001819
00704                  88  SKIP-SEPTEMBER         VALUE '3'.            00001820
00705                  88  SKIP-JULY-AUG          VALUE '4'.            00001821
00706                  88  SKIP-AUG-SEPT          VALUE '5'.            00001822
00707                  88  SKIP-JULY-AUG-SEPT     VALUE '6'.            00001823
00708              20  EX-FB-PAYMENT-MODE         PIC X.                00001824
00709                  88  PAY-MONTHLY            VALUE SPACE.          00001825
00710                  88  PAY-WEEKLY             VALUE '1'.            00001826
00711                  88  PAY-SEMI-MONTHLY       VALUE '2'.            00001827
00712                  88  PAY-BI-WEEKLY          VALUE '3'.            00001828
00713                  88  PAY-SEMI-ANUALLY       VALUE '4'.            00001829
00714                                                                   00001830
00715              20  EX-FB-LOAN-NUMBER          PIC X(8).             00001831
00716              20  EX-FB-LOAN-BALANCE         PIC S9(7)V99  COMP-3. 00001832
00717                                                                   00001833
00718              20  EX-FB-REIN-TABLE           PIC X(3).             00001834
00719              20  EX-FB-SPECIAL-REIN-CODE    PIC X.                00001835
00720                                                                   00001836
00721          16  EX-FB-STATUS-DATA.                                   00001837
00722              20  EX-FB-AH-CANCEL-DT             PIC XX.           00001838
00723              20  EX-FB-LF-CANCEL-DT             PIC XX.           00001839
00724              20  EX-FB-AH-SETTLEMENT-DT         PIC XX.           00001840
00725              20  EX-FB-LF-DEATH-DT              PIC XX.           00001841
00726              20  EX-FB-ENTRY-DT                 PIC XX.           00001842
LGC003*            20  EX-FB-AH-EXIT-DT               PIC XX.           00001843
LGC003*            20  EX-FB-LF-EXIT-DT               PIC XX.           00001844
00728              20  EX-FB-AH-CANCEL-EXIT-DT        PIC XX.           00001845
00728              20  EX-FB-AH-SETTLEMENT-EXIT-DT    PIC XX.           00001846
00728              20  EX-FB-LF-CANCEL-EXIT-DT        PIC XX.           00001847
00728              20  EX-FB-LF-DEATH-EXIT-DT         PIC XX.           00001848
00729              20  EX-FB-LF-CURRENT-STATUS        PIC X.            00001849
00730                  88  EX-FB-LF-POLICY-IS-ACTIVE     VALUE '1' '2'  00001850
00731                                                  '3' '4' '5' '9'. 00001851
00732                  88  EX-FB-LF-NORMAL-ENTRY         VALUE '1'.     00001852
00733                  88  EX-FB-LF-POLICY-PENDING       VALUE '2'.     00001853
00734                  88  EX-FB-LF-POLICY-IS-RESTORE    VALUE '3'.     00001854
00735                  88  EX-FB-LF-CONVERSION-ENTRY     VALUE '4'.     00001855
00736                  88  EX-FB-LF-POLICY-IS-REISSUE    VALUE '5'.     00001856
00737                  88  EX-FB-LF-LUMP-SUM-DISAB       VALUE '6'.     00001857
00738                  88  EX-FB-LF-DEATH-CLAIM-APPLIED  VALUE '7'.     00001858
00739                  88  EX-FB-LF-CANCEL-APPLIED       VALUE '8'.     00001859
00740                  88  EX-FB-LF-IS-REIN-ONLY         VALUE '9'.     00001860
00741                                                                   00001861
00742              20  EX-FB-LF-ENTRY-STATUS      PIC X.                00001862
00743              20  EX-FB-LF-STATUS-AT-DEATH   PIC X.                00001863
00744              20  EX-FB-LF-STATUS-AT-CANCEL  PIC X.                00001864
00745                                                                   00001865
00746              20  EX-FB-AH-CURRENT-STATUS    PIC X.                00001866
00747                  88  EX-FB-AH-POLICY-IS-ACTIVE     VALUE '1' '2'  00001867
00748                                                  '3' '4' '5' '9'. 00001868
00749                  88  EX-FB-AH-NORMAL-ENTRY         VALUE '1'.     00001869
00750                  88  EX-FB-AH-POLICY-PENDING       VALUE '2'.     00001870
00751                  88  EX-FB-AH-POLICY-IS-RESTORE    VALUE '3'.     00001871
00752                  88  EX-FB-AH-CONVERSION-ENTRY     VALUE '4'.     00001872
00753                  88  EX-FB-AH-POLICY-IS-REISSUE    VALUE '5'.     00001873
00754                  88  EX-FB-AH-LUMP-SUM-DISAB       VALUE '6'.     00001874
00755                  88  EX-FB-AH-DEATH-CLAIM-APPLIED  VALUE '7'.     00001875
00756                  88  EX-FB-AH-CANCEL-APPLIED       VALUE '8'.     00001876
00757                  88  EX-FB-AH-IS-REIN-ONLY         VALUE '9'.     00001877
00758              20  EX-FB-AH-ENTRY-STATUS       PIC X.               00001878
00759              20  EX-FB-AH-STATUS-AT-LUMP-SUM PIC X.               00001879
00760              20  EX-FB-AH-STATUS-AT-CANCEL   PIC X.               00001880
00761                                                                   00001881
00762              20  EX-FB-CLAIM-INTERFACE-SW  PIC X.                 00001882
00763                  88  EX-FB-NO-CLAIM-ATTACHED          VALUE SPACE.00001883
00764                  88  EX-FB-CERT-AND-CLAIM-ONLINE      VALUE '1'.  00001884
00765                  88  EX-FB-CERT-WAS-CREATED           VALUE '2'.  00001885
00766                                                                   00001886
00767              20  EX-FB-CLAIM-ATTACHED-COUNT PIC S9(4)       COMP. 00001887
00768                                                                   00001888
00769              20  EX-FB-ENTRY-BATCH          PIC X(6).             00001889
00770              20  EX-FB-LAST-MONTH-END       PIC XX.               00001890
00771                                                                   00001891
00772              20  EX-FB-AH-PAID-THRU-DT      PIC XX.               00001892
00773                  88  EX-FB-NO-AH-CLAIMS-PAID    VALUE LOW-VALUE.  00001893
00774                                                                   00001894
00775          16  EX-FB-CREDIT-INTERFACE-SW-1    PIC X.                00001895
00776              88  EX-FB-CERT-ADDED-BATCH           VALUE ' '.      00001896
00777              88  EX-FB-CERT-ADDED-ONLINE          VALUE '1'.      00001897
00778              88  EX-FB-CERT-PEND-ISSUE-ERROR      VALUE '2'.      00001898
00779              88  EX-FB-CERT-PURGED-OFFLINE        VALUE '3'.      00001899
00780          16  EX-FB-CREDIT-INTERFACE-SW-2    PIC X.                00001900
00781              88  EX-FB-CERT-AS-LOADED             VALUE ' '.      00001901
00782              88  EX-FB-CERT-CANCELLED-ONLINE      VALUE '1'.      00001902
00783              88  EX-FB-CERT-CLAIM-ONLINE          VALUE '2'.      00001903
00784              88  EX-FB-CERT-CLAIM-CANCEL-ONLINE   VALUE '3'.      00001904
00785              88  EX-FB-CERT-PEND-CANCEL-ERROR     VALUE '4'.      00001905
00786              88  EX-FB-CERT-PEND-CANCEL-VOID      VALUE '5'.      00001906
00787              88  EX-FB-CERT-PEND-CAN-VOID-ERROR   VALUE '6'.      00001907
00788                                                                   00001908
00789          16  EX-FB-ACCOUNT-COMM-PCTS.                             00001909
00790              20  EX-FB-LIFE-COMM-PCT        PIC SV9(5)    COMP-3. 00001910
00791              20  EX-FB-AH-COMM-PCT          PIC SV9(5)    COMP-3. 00001911
LGC003*        16  FILLER                         PIC X(57).            00001912
00792                                                                   00001913
LGC003         16  CSO-FILLER-CHG.                                      00001914
LGC003             20  EX-FB-GROUPING               PIC X(06).          00001915
LGC003             20  EX-FB-STATE                  PIC X(02).          00001916
LFC003             20  EX-FB-INSURED-LAST-NAME      PIC X(15).          00001917
LGC003             20  EX-FB-INSURED-INITIALS       PIC XX.             00001918
LGC003             20  FILLER                       PIC X(32).          00001919
00794                                                                   00001920
00795      12  EX-EXTRACT-F-RECORD-C  REDEFINES  EX-DATA-AREAS.         00001921
00796          16  EX-FC-INSURED-NAME              PIC X(30).           00001922
00797          16  EX-FC-TRAILER-TYPE              PIC X.               00001923
00798              88  EX-FC-RESERVE-EXPENSE-TR         VALUE '1'.      00001924
00799              88  EX-FC-PAYMENT-TR                 VALUE '2'.      00001925
00800              88  EX-FC-AUTO-PAY-TR                VALUE '3'.      00001926
00801              88  EX-FC-CORRESPONDENCE-TR          VALUE '4'.      00001927
00802              88  EX-FC-ADDRESS-TR                 VALUE '5'.      00001928
00803              88  EX-FC-GENERAL-INFO-TR            VALUE '6'.      00001929
00804              88  EX-FC-AUTO-PROMPT-TR             VALUE '7'.      00001930
00805              88  EX-FC-DENIAL-TR                  VALUE '8'.      00001931
00806              88  EX-FC-INCURRED-CHG-TR            VALUE '9'.      00001932
00807              88  EX-FC-FORM-CONTROL-TR            VALUE 'A'.      00001933
00808          16  EX-FC-RECORDED-DT               PIC XX.              00001934
00809          16  EX-FC-RECORDED-BY               PIC X(4).            00001935
00810          16  EX-FC-LAST-MAINT-HHMMSS         PIC S9(6)    COMP-3. 00001936
00811          16  FILLER                          PIC X(66).           00001937
00812          16  EX-FC-TRAILER-BODY              PIC X(165).          00001938
00813                                                                   00001939
LGC003                                                                  00001940
LGC003                                                                  00001941
LGC003 01  WS-CM-HOLD.                                                  00001942
LGC003     05  WS-CM-CD-A1          PIC X.                              00001943
LGC003     05  WS-CM-NAME.                                              00001944
LGC003         10  WS-CM-LAST       PIC X(15).                          00001945
LGC003         10  WS-CM-INITS      PIC X(02).                          00001946
LGC003                                                                  00001947
                                                                        00001948
      ******************************************************************00004390
                                                                        00004400
       01  REPORT-AREAS.                                                00004410
           05  WS-HEADING1.                                             00004420
               10  FILLER           PIC X(45)   VALUE SPACES.           00004430
               10  FILLER           PIC X(29)   VALUE                   00004440
                   'LOGIC CLAIMS ACTIVITY EXTRACT'.                     00004450
               10  FILLER           PIC X(26)   VALUE SPACES.           00004460
               10  FILLER           PIC X(16)   VALUE                   00004470
                   'REPORT - CICLEXT'.                                  00004471
               10  FILLER           PIC X(16)   VALUE SPACES.           00004490
                                                                        00004500
           05  WS-HEADING2.                                             00004510
               10  FILLER           PIC X(50)   VALUE SPACES.           00004520
               10  FILLER           PIC X(11)   VALUE                   00004530
                   'RUN DATE - '.                                       00004540
               10  WS-HDG-RUN-DATE  PIC X(10).                          00004550
               10  FILLER           PIC X(31)   VALUE SPACES.           00004560
               10  FILLER           PIC X(7)    VALUE                   00004570
                   'PAGE - '.                                           00004580
               10  WS-HDG-PAGE      PIC 9(4).                           00004590
               10  FILLER           PIC X(20)   VALUE SPACES.           00004600
                                                                        00004610
           05  WS-HEADING3.                                             00004620
               10  FILLER           PIC X(10)   VALUE SPACES.           00004630
               10  FILLER           PIC X(7)    VALUE                   00004640
                   'COMPANY'.                                           00004650
               10  FILLER           PIC X(7)    VALUE SPACES.           00004660
               10  FILLER           PIC X(7)    VALUE                   00004670
                   'CARRIER'.                                           00004680
               10  FILLER           PIC X(7)    VALUE SPACES.           00004690
               10  FILLER           PIC X(12)   VALUE                   00004700
                   'CLAIM NUMBER'.                                      00004710
               10  FILLER           PIC X(6)    VALUE SPACES.           00004720
               10  FILLER           PIC X(11)   VALUE                   00004730
                   'CERT NUMBER'.                                       00004740
               10  FILLER           PIC X(7)    VALUE SPACES.           00004750
               10  FILLER           PIC X(13)   VALUE                   00004760
                   '# OF TRAILERS'.                                     00004770
               10  FILLER           PIC X(7)    VALUE SPACES.           00004780
               10  FILLER           PIC X(12)   VALUE                   00004790
                   'ACTION TAKEN'.                                      00004800
               10  FILLER           PIC X(26)   VALUE SPACES.           00004810
                                                                        00004820
           05  WS-DETAIL-LINE.                                          00004830
               10  FILLER                      PICTURE X(12).           00004840
               10  WS-DET-COMPANY              PICTURE X(3).            00004850
               10  FILLER                      PICTURE X(12).           00004860
               10  WS-DET-CARRIER              PICTURE X(1).            00004870
               10  FILLER                      PICTURE X(12).           00004880
               10  WS-DET-CLAIM-NO             PICTURE X(7).            00004890
               10  FILLER                      PICTURE X(11).           00004900
LGC003         10  WS-DET-CERT-NO              PICTURE X(11).           00004901
LGC003         10  FILLER                      PICTURE X(8).            00004902
               10  WS-DET-TRLRS                PICTURE Z(5).            00004930
               10  FILLER                      PICTURE X(14).           00004940
               10  WS-DET-ACTION               PICTURE X(15).           00004950
               10  FILLER                      PICTURE X(21).           00004960
                                                                        00004970
           EJECT                                                        00004980
                                                                        00004990
       LINKAGE SECTION.                                                 00005000
       01  D-OS-PARM-D-OS.                                              00005001
           05  D-OS-PARM-LENTGH             PIC S9(4)    COMP.          00005002
           05  D-0S-PARM-DATA.                                          00005003
               10 CURRENT-DATE-IN           PIC X(10).                  00005004
               10 FILLER                    PIC X(90).                  00005005
                                                                        00005020
           EJECT                                                        00005030
       PROCEDURE DIVISION  USING  D-OS-PARM-D-OS.                       00005040
       0000-MAIN-LOGIC SECTION.                                         00005050

           PERFORM 1000-OPEN-FILES.                                     00005060
      *    MOVE '667685570006493864 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
      *    MOVE '982769150006996172 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
      *    MOVE '987790020006785977 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
      *    MOVE '915966580100319394 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
      *    MOVE '90B932550006521190 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
      *    MOVE '931073840006687677 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'V'                    TO DA-RECORD-TYPE
      *    PERFORM 2000-PROCESSING-LOOP                                 00005070
           MOVE '42C000040000000034 '  TO DA-KEY (2:19)
           MOVE X'05'                  TO DA-KEY (1:1)
           MOVE 'V'                    TO DA-RECORD-TYPE
           PERFORM 2000-PROCESSING-LOOP                                 00005070
           MOVE '431000050000000013 '  TO DA-KEY (2:19)
           MOVE X'05'                  TO DA-KEY (1:1)
           MOVE 'V'                    TO DA-RECORD-TYPE
           PERFORM 2000-PROCESSING-LOOP                                 00005070
      *        UNTIL DLYACTV-EOF.                                       00005080
           PERFORM 9000-CLOSE-FILES.                                    00005090
           GOBACK.                                                      00005100
                                                                        00005110
           EJECT                                                        00005120
       1000-OPEN-FILES SECTION.                                         00005130
LGC003     OPEN INPUT ELMSTR                                            00005131
LGC003                ELCERT                                            00005132
LGC003                ELTRLR                                            00005133
LGC003                ERACCT                                            00005134
PEMTST*         I-O   DLYACTV                                           00005180
LGC003          OUTPUT CIEXTRS                                          00005181
                       PRNTR.                                           00005200
           MOVE 'ERROR OCCURED OPENING -'  TO  WS-ERROR-PART1           00005210
           IF DLYACTV-FILE-STATUS     EQUAL ZERO  OR  '97'              00005211
               NEXT SENTENCE                                            00005230
             ELSE                                                       00005240
               MOVE 'DLYACTV '         TO  WS-ERROR-PART2               00005250
               MOVE DLYACTV-FILE-STATUS TO  WS-ERROR-STATUS             00005260
               PERFORM 9999-ABEND-PGM.                                  00005270
LGC003     IF ELMSTR-FILE-STATUS     EQUAL ZERO  OR  '97'               00005271
               NEXT SENTENCE                                            00005290
             ELSE                                                       00005300
LGC003         MOVE 'ELMSTR  '         TO  WS-ERROR-PART2               00005301
LGC003         MOVE ELMSTR-FILE-STATUS TO  WS-ERROR-STATUS              00005302
               PERFORM 9999-ABEND-PGM.                                  00005330
LGC003     IF ELCERT-FILE-STATUS     EQUAL ZERO     OR  '97'            00005331
               NEXT SENTENCE                                            00005350
             ELSE                                                       00005360
LGC003         MOVE 'ELCERT  '         TO  WS-ERROR-PART2               00005361
LGC003         MOVE ELCERT-FILE-STATUS TO  WS-ERROR-STATUS              00005362
               PERFORM 9999-ABEND-PGM.                                  00005390
LGC003     IF ELTRLR-FILE-STATUS     EQUAL ZERO  OR  '97'               00005391
               NEXT SENTENCE                                            00005410
             ELSE                                                       00005420
LGC003         MOVE 'ELTRLR  '         TO  WS-ERROR-PART2               00005421
LGC003         MOVE ELTRLR-FILE-STATUS TO  WS-ERROR-STATUS              00005422
               PERFORM 9999-ABEND-PGM.                                  00005450
LGC003     IF ERACCT-FILE-STATUS     EQUAL ZERO   OR  '97'              00005451
               NEXT SENTENCE                                            00005470
             ELSE                                                       00005480
LGC003         MOVE 'ERACCT  '         TO  WS-ERROR-PART2               00005481
LGC003         MOVE ERACCT-FILE-STATUS TO  WS-ERROR-STATUS              00005482
               PERFORM 9999-ABEND-PGM.                                  00005510
CSOMOD                                                                  00005520
CSOMOD     DISPLAY 'CURRENT-DATE-IN = ' CURRENT-DATE-IN.                00005520
CSOMOD                                                                  00005520
CSOMOD     MOVE CURRENT-DATE-IN TO WS-RUN-DATE.                         00005520
CSOMOD*    MOVE CURRENT-DATE TO WS-RUN-DATE.                            00005530
           MOVE +0  TO WS-ZERO-R.                                       00005540
           MOVE +4  TO WS-FOUR-R.                                       00005550
           MOVE +5  TO WS-FIVE-R.                                       00005560
       1000-EXIT.                                                       00005570
           EXIT.                                                        00005580
                                                                        00005590
           EJECT                                                        00005600
       2000-PROCESSING-LOOP SECTION.                                    00005610
           PERFORM 3100-READ-DLYACTV THRU 3100-EXIT.                    00005620
           IF NOT DLYACTV-EOF                                           00005630
               NEXT SENTENCE                                            00005640
           ELSE                                                         00005650
               GO TO 2000-EXIT.                                         00005660
           MOVE X'04' TO WS-COMPANY-CD.                                 00005670
           IF WS-COMPANY-CD = WS-ZERO-P2                                00005680
               GO TO 2000-EXIT.                                         00005690
           ADD +1 TO RECORDS-IN.                                        00005700
           IF WS-COMPANY-CD = WS-FOUR-P2                                00005710
               MOVE 'CID' TO WS-COMPANY-ID.                             00005720
           IF WS-COMPANY-CD = WS-FIVE-P2                                00005730
               MOVE 'DMD' TO WS-COMPANY-ID.                             00005740
      *    MOVE '922012890350007332 '  TO DA-KEY (2:19)
      *    MOVE X'04'                  TO DA-KEY (1:1)
      *    MOVE 'P'                    TO DA-RECORD-TYPE
      *                                   WS-HOLD-KEY (2:19)
      *    MOVE X'04'                  TO CL-COMPANY-CD
      *                                   WS-HOLD-KEY (1:1)
           MOVE DA-KEY TO CL-CONTROL-PRIMARY                            00005750
                          WS-HOLD-KEY.                                  00005760
CIDMOD                                                                  00005710
CIDMOD*    DISPLAY' WS-COMPANY-CD = ' WS-COMPANY-ID.                    00005710
CIDMOD                                                                  00005710
           MOVE +0     TO WS-CURRENT-TRLRS.                             00005770
LGC003     PERFORM 3200-READ-ELMSTR THRU 3200-EXIT.                     00005771
           IF CLAIM-NOT-FOUND                                           00005790
               GO TO 2000-CONTINUE.                                     00005800
           PERFORM 4000-OUTPUT-FA-RECORD THRU 4000-EXIT.                00005810
LGC003     PERFORM 3300-READ-ELCERT THRU 3300-EXIT.                     00005811
           PERFORM 4100-OUTPUT-FB-RECORD THRU 4100-EXIT.                00005830
LGC003     PERFORM 3400-READ-ELTRLR THRU 3400-EXIT.                     00005831
       2000-CONTINUE.                                                   00005850
           PERFORM 3600-DELETE-DLYACTV THRU 3600-EXIT.                  00005860
           PERFORM 4300-OUTPUT-REPORT THRU 4300-EXIT.
      *    SET DLYACTV-EOF TO TRUE

           .
       2000-EXIT.                                                       00005880
           EXIT.                                                        00005890
                                                                        00005900
       3100-READ-DLYACTV SECTION.                                       00005910
      *    READ DLYACTV NEXT                                            00005920
      *        AT END                                                   00005930
      *            MOVE 'Y' TO DLYACTV-EOF-SW.                          00005940
      *    IF DLYACTV-EOF                                               00005950
      *        GO TO 3100-EXIT.                                         00005960
      *    IF DLYACTV-FILE-STATUS NOT EQUAL TO ZERO                     00005970
      *        MOVE 'ERROR OCCURED READ  -  '  TO WS-ERROR-PART1        00005980
      *        MOVE 'DLYACTV '                 TO WS-ERROR-PART2        00005990
      *        MOVE DLYACTV-FILE-STATUS TO  WS-ERROR-STATUS             00006000
      *        PERFORM 9999-ABEND-PGM.                                  00006010
DAN        MOVE 'Y' TO DLYACTV-DELETE-SW.                               00005940
       3100-EXIT.                                                       00006020
           EXIT.                                                        00006030
                                                                        00006040
LGC003 3200-READ-ELMSTR SECTION.                                        00006041
           MOVE 'N' TO  WS-CLAIM-FOUND-SW.                              00006060
LGC003     READ ELMSTR.                                                 00006061
LGC003     IF ELMSTR-FILE-STATUS EQUAL TO '23'                          00006062
               MOVE 'N'                TO  WS-CLAIM-FOUND-SW            00006090
               GO TO 3200-EXIT.                                         00006100
LGC003     IF ELMSTR-FILE-STATUS NOT EQUAL TO ZERO                      00006101
CIDMOD         DISPLAY 'ERROR OCCURED READ ELMSTR  FILE'                00005980
               MOVE 'ERROR OCCURED READ  -  '  TO WS-ERROR-PART1        00006120
LGC003         MOVE 'ELMSTR  '                 TO WS-ERROR-PART2        00006121
LGC003         MOVE ELMSTR-FILE-STATUS TO  WS-ERROR-STATUS              00006122
               PERFORM 9999-ABEND-PGM.                                  00006150
           MOVE 'Y'                TO  WS-CLAIM-FOUND-SW.               00006160
       3200-EXIT.                                                       00006170
           EXIT.                                                        00006180
                                                                        00006190
LGC003 3300-READ-ELCERT SECTION.                                        00006191
           MOVE WS-COMPANY-CD    TO CM-COMPANY-CD.                      00006210
           MOVE CL-CERT-CARRIER  TO CM-CARRIER.                         00006220
           MOVE CL-CERT-GROUPING TO CM-GROUPING.                        00006230
           MOVE CL-CERT-STATE    TO CM-STATE.                           00006240
           MOVE CL-CERT-ACCOUNT  TO CM-ACCOUNT.                         00006250
           MOVE CL-CERT-EFF-DT   TO CM-CERT-EFF-DT.                     00006260
           MOVE CL-CERT-NO       TO CM-CERT-NO.                         00006270
LGC003     READ ELCERT.                                                 00006271
LGC003     IF ELCERT-FILE-STATUS EQUAL TO '23'                          00006272
               GO TO 3300-EXIT.                                         00006300
LGC003     IF ELCERT-FILE-STATUS NOT EQUAL TO ZERO                      00006301
CIDMOD         DISPLAY 'ERROR OCCURED READ ELCERT  FILE'                00005980
               MOVE 'ERROR OCCURED READ  -  '  TO WS-ERROR-PART1        00006320
LGC003         MOVE 'ELCERT  '                 TO WS-ERROR-PART2        00006321
LGC003         MOVE ELCERT-FILE-STATUS TO  WS-ERROR-STATUS              00006322
               PERFORM 9999-ABEND-PGM.                                  00006350
       3300-EXIT.                                                       00006360
           EXIT.                                                        00006370
                                                                        00006380
LGC003 3400-READ-ELTRLR SECTION.                                        00006381
           MOVE DA-KEY   TO AT-CONTROL-PRIMARY.                         00006400
           MOVE ZERO     TO AT-SEQUENCE-NO.                             00006410
LGC003     START ELTRLR                                                 00006411
               KEY IS NOT LESS THAN AT-CONTROL-PRIMARY.                 00006430
LGC003     IF ELTRLR-FILE-STATUS EQUAL TO '23'                          00006431
               GO TO 3400-EXIT.                                         00006450
LGC003     IF ELTRLR-FILE-STATUS NOT EQUAL TO ZERO                      00006451
CIDMOD         DISPLAY 'ERROR OCCURED START ELTRLR  FILE'               00005980
               MOVE 'ERROR OCCURED START -  '  TO WS-ERROR-PART1        00006470
LGC003         MOVE 'ELTRLR  '                 TO WS-ERROR-PART2        00006471
LGC003         MOVE ELTRLR-FILE-STATUS TO  WS-ERROR-STATUS              00006472
               PERFORM 9999-ABEND-PGM.                                  00006500
       3410-GET-TRAILER.                                                00006510
LGC003     READ ELTRLR NEXT.                                            00006511
LGC003     IF ELTRLR-FILE-STATUS EQUAL TO '10'                          00006512
               GO TO 3400-EXIT.                                         00006540
LGC003     IF ELTRLR-FILE-STATUS NOT EQUAL TO ZERO                      00006541
CIDMOD         DISPLAY 'ERROR OCCURED READNEXT ELTRLR  FILE'            00005980
               MOVE 'ERROR OCCURED READNEXT-'  TO WS-ERROR-PART1        00006560
LGC003         MOVE ' ELTRLR '                 TO WS-ERROR-PART2        00006561
LGC003         MOVE ELTRLR-FILE-STATUS TO  WS-ERROR-STATUS              00006562
               PERFORM 9999-ABEND-PGM.                                  00006590
           MOVE AT-CONTROL-PRIMARY TO WS-CHECK-TRLR-KEY.                00006600
           IF WS-CHECK-KEY NOT = WS-HOLD-KEY                            00006610
               GO TO 3400-EXIT.                                         00006620


DAN        IF AT-TRAILER-TYPE = '2'
DAN           IF AT-CHECK-WRITTEN-DT = LOW-VALUES
DAN              MOVE 'N' TO DLYACTV-DELETE-SW
DAN   *          GO TO 3400-EXIT.                                       00006640
PEMMOD           GO TO 3410-GET-TRAILER.                                00006640

           IF (AT-TRAILER-TYPE = '2')
              AND (AT-CHECK-NO = '4000007' OR '4000005')
              PERFORM 4200-OUTPUT-FC-RECORD THRU 4200-EXIT
           END-IF
           
           GO TO 3410-GET-TRAILER.                                      00006640
       3400-EXIT.                                                       00006650
           EXIT.                                                        00006660
                                                                        00006670
LGC003 3500-READ-ERACCT SECTION.                                        00006671
           MOVE 'N' TO  WS-ACCOUNT-FOUND-SW.                            00006690
           MOVE WS-COMPANY-CD          TO  AM-COMPANY-CD.               00006700
           MOVE CL-CERT-CARRIER        TO  AM-CARRIER.                  00006710
           MOVE CL-CERT-GROUPING       TO  AM-GROUPING.                 00006720
           MOVE CL-CERT-STATE          TO  AM-STATE.                    00006730
           MOVE CL-CERT-ACCOUNT        TO  AM-ACCOUNT.                  00006740
           MOVE CL-CERT-EFF-DT         TO  AM-EXPIRATION-DT.            00006750
LGC003     START ERACCT                                                 00006751
               KEY IS NOT LESS THAN AM-CONTROL-PRIMARY.                 00006770
LGC003     IF ERACCT-FILE-STATUS EQUAL TO '23'                          00006771
               MOVE 'N'                TO  WS-ACCOUNT-FOUND-SW          00006790
               GO TO 3500-EXIT.                                         00006800
LGC003     IF ERACCT-FILE-STATUS NOT EQUAL TO ZERO                      00006801
CIDMOD         DISPLAY 'ERROR OCCURED START ERACCT  FILE'               00005980
               MOVE 'ERROR OCCURED START -  '  TO WS-ERROR-PART1        00006820
LGC003         MOVE 'ERACCT  '                 TO WS-ERROR-PART2        00006821
LGC003         MOVE ERACCT-FILE-STATUS TO  WS-ERROR-STATUS              00006822
               PERFORM 9999-ABEND-PGM.                                  00006850
       3510-GET-ACCOUNT.                                                00006860
LGC003     READ ERACCT NEXT.                                            00006861
LGC003     IF ERACCT-FILE-STATUS EQUAL TO '10'                          00006862
               MOVE 'N'                TO  WS-ACCOUNT-FOUND-SW          00006890
               GO TO 3500-EXIT.                                         00006900
LGC003     IF ERACCT-FILE-STATUS NOT EQUAL TO ZERO                      00006901
CIDMOD         DISPLAY 'ERROR OCCURED READNEXT ERACCT  FILE'            00005980
               MOVE 'ERROR OCCURED READNEXT-'  TO WS-ERROR-PART1        00006920
LGC003         MOVE ' ERACCT '                 TO WS-ERROR-PART2        00006921
LGC003         MOVE ERACCT-FILE-STATUS TO  WS-ERROR-STATUS              00006922
               PERFORM 9999-ABEND-PGM.                                  00006950
           IF AM-COMPANY-CD NOT EQUAL TO WS-COMPANY-CD                  00006960
             OR AM-CARRIER NOT EQUAL TO CL-CERT-CARRIER                 00006970
             OR AM-GROUPING NOT EQUAL TO CL-CERT-GROUPING               00006980
             OR AM-STATE NOT EQUAL TO CL-CERT-STATE                     00006990
             OR AM-ACCOUNT NOT EQUAL TO CL-CERT-ACCOUNT                 00007000
               MOVE 'N'                TO  WS-ACCOUNT-FOUND-SW          00007010
               GO TO 3500-EXIT.                                         00007020
           IF CL-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT              00007030
             AND CL-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT              00007040
               MOVE 'Y'                TO  WS-ACCOUNT-FOUND-SW          00007050
               GO TO 3500-EXIT.                                         00007060
           GO TO 3510-GET-ACCOUNT.                                      00007070
       3500-EXIT.                                                       00007080
           EXIT.                                                        00007090
                                                                        00007100
       3600-DELETE-DLYACTV SECTION.                                     00007110
DAN   *    IF DLYACTV-DELETE-SW = 'Y'
      *       DELETE DLYACTV RECORD                                        00007
      *       IF DLYACTV-FILE-STATUS NOT EQUAL TO ZERO                     00007
      *           MOVE 'ERROR OCCURED DELETE - ' TO WS-ERROR-PART1         00007
      *           MOVE 'DLYACTV '                TO WS-ERROR-PART2         00007
      *           MOVE DLYACTV-FILE-STATUS       TO WS-ERROR-STATUS        00007
      *           PERFORM 9999-ABEND-PGM                                   00007
      *       END-IF                                                       00007
      *    END-IF.                                                         00007
       3600-EXIT.                                                       00007180
           EXIT.                                                        00007190
                                                                        00007200
       4000-OUTPUT-FA-RECORD SECTION.                                   00007210
                                                                        00007220
LGC003     MOVE SPACES                 TO  EX-EXTRACT-F-RECORD-A.       00007221
LGC003                                                                  00007222
           MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD       00007230
                                                                        00007240
           MOVE '9'                    TO  EX-POSITIONING-CODE          00007250
           MOVE 'F'                    TO  EX-EXTRACT-CODE              00007260
           MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD                00007270
           MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                00007280
           MOVE 'A'                    TO  EX-RECORD-TYPE               00007290
                                                                        00007300
           MOVE CL-CARRIER             TO  EX-SG-CARRIER                00007310
           MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO             00007320
           MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO               00007330
           MOVE CL-CERT-NO             TO  EX-SG-CERT-NO                00007340
           MOVE ZERO                   TO  EX-SG-TRAILER-SEQ-NO         00007350
                                                                        00007360
           PERFORM 5000-MOVE-NAME.                                      00007370
LGC003     MOVE WS-NAME-WORK           TO  EX-FA-INSURED-NAME.          00007371
LGC003     MOVE SPACES                 TO  EX-FA-ACCOUNT-NAME.          00007372
LGC003     MOVE CL-INSURED-BIRTH-DT    TO  EX-FA-INSURED-BIRTH-DT.      00007373
LGC003     MOVE CL-INSURED-SEX-CD      TO  EX-FA-INSURED-SEX-CD.        00007374
LGC003     MOVE CL-INSURED-OCC-CD      TO  EX-FA-INSURED-OCC-CD.        00007375
LGC003     MOVE CL-SOC-SEC-NO          TO  EX-FA-SOC-SEC-NO.            00007376
LGC003     MOVE CL-PROCESSOR-ID        TO  EX-FA-PROCESSOR-ID.          00007377
LGC003     MOVE CL-CLAIM-STATUS        TO  EX-FA-CLAIM-STATUS.          00007378
LGC003     MOVE CL-CLAIM-TYPE          TO  EX-FA-CLAIM-TYPE.            00007379
LGC003     MOVE CL-CLAIM-PREM-TYPE     TO  EX-FA-CLAIM-PREM-TYPE.       00007380
LGC003     MOVE CL-INCURRED-DT         TO  EX-FA-INCURRED-DT.           00007381
LGC003     MOVE CL-REPORTED-DT         TO  EX-FA-REPORTED-DT.           00007382
LGC003     MOVE CL-FILE-ESTABLISH-DT   TO  EX-FA-FILE-ESTABLISH-DT.     00007383
LGC003     MOVE CL-EST-END-OF-DISAB-DT TO  EX-FA-EST-END-OF-DISAB-DT.   00007384
LGC003     MOVE CL-LAST-PMT-DT         TO  EX-FA-LAST-PMT-DT.           00007385
LGC003     MOVE CL-LAST-PMT-AMT        TO  EX-FA-LAST-PMT-AMT.          00007386
LGC003     MOVE CL-PAID-THRU-DT        TO  EX-FA-PAID-THRU-DT.          00007387
LGC003     MOVE CL-TOTAL-PAID-AMT      TO  EX-FA-TOTAL-PAID-AMT.        00007388
LGC003     MOVE CL-NO-OF-PMTS-MADE     TO  EX-FA-NO-OF-PMTS-MADE.       00007389
LGC003     MOVE CL-NO-OF-DAYS-PAID     TO  EX-FA-NO-OF-DAYS-PAID.       00007390
LGC003     MOVE CL-PMT-CALC-METHOD     TO  EX-FA-PMT-CALC-METHOD.       00007391
LGC003     MOVE CL-CAUSE-CD            TO  EX-FA-CAUSE-CD.              00007392
LGC003     MOVE SPACES                 TO  EX-FA-DIAGNOSIS-DESCRIP.     00007393
LGC003     MOVE CL-LAST-REOPEN-DT      TO  EX-FA-LAST-REOPEN-DT.        00007394
LGC003     MOVE CL-LAST-CLOSE-DT       TO  EX-FA-LAST-CLOSE-DT.         00007395
LGC003     MOVE CL-LAST-CLOSE-REASON   TO  EX-FA-LAST-CLOSE-REASON.     00007396
LGC003     MOVE CL-CERT-ORIGIN         TO  EX-FA-CERT-ORIGIN.           00007397
LGC003     MOVE CL-PRIORITY-CD         TO  EX-FA-PRIORITY-CD.           00007398
LGC003     MOVE CL-SUPV-ATTN-CD        TO  EX-FA-SUPV-ATTN-CD.          00007399
LGC003     MOVE CL-PURGED-DT           TO  EX-FA-PURGED-DT.             00007400
LGC003     MOVE CL-RESTORED-DT         TO  EX-FA-RESTORED-DT.           00007401
LGC003     MOVE CL-NEXT-AUTO-PAY-DT    TO  EX-FA-NEXT-AUTO-PAY-DT.      00007402
LGC003     MOVE CL-NEXT-RESEND-DT      TO  EX-FA-NEXT-RESEND-DT.        00007403
LGC003     MOVE CL-NEXT-FOLLOWUP-DT    TO  EX-FA-NEXT-FOLLOWUP-DT.      00007404
LGC003     MOVE CL-LAST-MAINT-DT       TO  EX-FA-LAST-MAINT-DT.         00007405
LGC003     MOVE CL-LAST-MAINT-USER     TO  EX-FA-LAST-MAINT-USER.       00007406
LGC003     MOVE CL-LAST-MAINT-HHMMSS   TO  EX-FA-LAST-MAINT-HHMMSS.     00007407
LGC003     MOVE CL-LAST-MAINT-TYPE     TO  EX-FA-LAST-MAINT-TYPE.       00007408
LGC003     MOVE CL-RELATED-CLAIM-NO    TO  EX-FA-RELATED-CLAIM-NO.      00007409
LGC003     MOVE CL-HISTORY-ARCHIVE-DT  TO  EX-FA-HISTORY-ARCHIVE-DT.    00007410
LGC003     MOVE CL-TRAILER-SEQ-CNT     TO  EX-FA-TRAILER-SEQ-CNT.       00007411
LGC003     MOVE CL-LAST-INC-DT-CHANGE  TO  EX-FA-LAST-INC-DT-CHANGE.    00007412
LGC003     MOVE CL-AUTO-PAY-SEQ        TO  EX-FA-AUTO-PAY-SEQ.          00007413
LGC003     MOVE CL-ACCOUNT-ADDR-CNT    TO  EX-FA-ACCOUNT-ADDR-CNT.      00007414
LGC003     MOVE CL-FILE-LOCATION       TO  EX-FA-FILE-LOCATION.         00007415
LGC003     MOVE CL-BENEFICIARY         TO  EX-FA-BENEFICIARY.           00007416
                                                                        00007440
                                                                        00007520
LGC003     PERFORM 3500-READ-ERACCT THRU 3500-EXIT.                     00007521
           IF ACCOUNT-FOUND                                             00007540
LGC003         MOVE AM-ADDRS            TO  EX-FA-ACCOUNT-NAME          00007541
           ELSE                                                         00007560
               MOVE 'ACCOUNT NOT FOUND' TO  EX-FA-ACCOUNT-NAME.         00007570
           PERFORM 6000-WRITE-EXTRACT-FILE THRU 6000-EXIT.              00007580
           ADD +1  TO  WS-FA-COUNT.                                     00007590
       4000-EXIT.                                                       00007600
           EXIT.                                                        00007610
                                                                        00007620
           EJECT                                                        00007630
       4100-OUTPUT-FB-RECORD SECTION.                                   00007640
                                                                        00007650
LGC003     MOVE SPACES                 TO  EX-EXTRACT-F-RECORD-B.       00007651
LGC003                                                                  00007652
           MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD.      00007653
                                                                        00007670
           MOVE '9'                    TO  EX-POSITIONING-CODE.         00007671
           MOVE 'F'                    TO  EX-EXTRACT-CODE.             00007672
           MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD.               00007673
           MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               00007674
           MOVE 'B'                    TO  EX-RECORD-TYPE.              00007675
                                                                        00007730
           MOVE CL-CARRIER             TO  EX-SG-CARRIER.               00007731
           MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO.            00007732
           MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO.              00007733
           MOVE CL-CERT-NO             TO  EX-SG-CERT-NO.               00007734
           MOVE ZERO                   TO  EX-SG-TRAILER-SEQ-NO.        00007735
LGC003                                                                  00007736
LGC003     MOVE CM-GROUPING            TO  EX-FB-GROUPING               00007737
LGC003     MOVE CM-STATE               TO  EX-FB-STATE                  00007738
LGC003     MOVE CM-CERT-EFF-DT         TO  EX-FB-EFFECTIVE-DT           00007739
LGC003     MOVE CM-INSURED-LAST-NAME   TO  EX-FB-INSURED-LAST-NAME      00007740
LGC003     MOVE CM-INSURED-INITIALS    TO  EX-FB-INSURED-INITIALS       00007741
LGC003     MOVE CM-SOC-SEC-NO          TO  EX-FB-SSN                    00007742
LGC003     MOVE CM-MEMBER-NO           TO  EX-FB-MEMBER-NO              00007743
LGC003     MOVE CM-INSURED-ISSUE-AGE   TO  EX-FB-INSURED-ISSUE-AGE      00007744
LGC003*    MOVE CM-SPOUSE-ISSUE-AGE    TO  EX-FB-SPOUSE-ISSUE-AGE       00007745
CIDMOD     MOVE CM-INSURED-JOINT-AGE   TO  EX-FB-SPOUSE-ISSUE-AGE       00007745
LGC003     MOVE CM-INSURED-SEX         TO  EX-FB-INSURED-SEX            00007746
LGC003     MOVE CM-LF-BENEFIT-CD       TO  EX-FB-LF-BENEFIT-CD.         00007747
LGC003     MOVE CM-LF-ORIG-TERM        TO  EX-FB-LF-ORIG-TERM.          00007748
LGC003     MOVE CM-LF-BENEFIT-AMT      TO  EX-FB-LF-BENEFIT-AMT.        00007749
LGC003     MOVE CM-LF-PREMIUM-AMT      TO  EX-FB-LF-PREMIUM-AMT.        00007750
LGC003     MOVE CM-LF-REMAINING-AMT    TO  EX-FB-LF-REMAINING-AMT.      00007751
LGC003     MOVE CM-LF-ITD-CANCEL-AMT   TO  EX-FB-LF-ITD-CANCEL-AMT.     00007752
LGC003     MOVE CM-LF-ITD-DEATH-AMT    TO  EX-FB-LF-ITD-DEATH-AMT.      00007753
LGC003*    MOVE SPACES                 TO  EX-FB-LF-FORM.               00007754
LGC003     MOVE CM-AH-BENEFIT-CD       TO  EX-FB-AH-BENEFIT-CD.         00007755
LGC003     MOVE CM-AH-ORIG-TERM        TO  EX-FB-AH-ORIG-TERM.          00007756
LGC003     MOVE CM-AH-BENEFIT-AMT      TO  EX-FB-AH-BENEFIT-AMT.        00007757
LGC003     MOVE CM-AH-PREMIUM-AMT      TO  EX-FB-AH-PREMIUM-AMT.        00007758
LGC003     MOVE CM-AH-ITD-CANCEL-AMT   TO  EX-FB-AH-ITD-CANCEL-AMT.     00007759
LGC003     MOVE CM-AH-ITD-LUMP-PMT     TO  EX-FB-AH-ITD-LUMP-PMT.       00007760
LGC003*    MOVE SPACES                 TO  EX-FB-AH-FORM.               00007761
LGC003     MOVE CM-LOAN-APR            TO  EX-FB-LOAN-APR.              00007762
LGC003     MOVE CM-PAY-FREQUENCY       TO  EX-FB-PAY-FREQUENCY.         00007763
LGC003     MOVE CM-LOAN-TERM           TO  EX-FB-LOAN-TERM.             00007764
LGC003     MOVE CM-RATE-CLASS          TO  EX-FB-RATE-CLASS.            00007765
LGC003*    MOVE SPACES                 TO  EX-FB-RATE-DEV.              00007766
LGC003     MOVE CM-PREMIUM-TYPE        TO  EX-FB-PREMIUM-TYPE.          00007767
LGC003     MOVE CM-IND-GRP-TYPE        TO  EX-FB-IND-GRP-TYPE.          00007768
LGC003     MOVE CM-SKIP-CODE           TO  EX-FB-SKIP-CODE.             00007769
LGC003     MOVE CM-PAYMENT-MODE        TO  EX-FB-PAYMENT-MODE.          00007770
LGC003     MOVE CM-LOAN-NUMBER         TO  EX-FB-LOAN-NUMBER.           00007771
LGC003     MOVE CM-LOAN-BALANCE        TO  EX-FB-LOAN-BALANCE.          00007772
LGC003     MOVE CM-REIN-TABLE          TO  EX-FB-REIN-TABLE.            00007773
LGC003     MOVE CM-SPECIAL-REIN-CODE   TO  EX-FB-SPECIAL-REIN-CODE.     00007774
LGC003     MOVE CM-AH-CANCEL-DT        TO  EX-FB-AH-CANCEL-DT.          00007775
LGC003     MOVE CM-LF-CANCEL-DT        TO  EX-FB-LF-CANCEL-DT.          00007776
LGC003     MOVE CM-AH-SETTLEMENT-DT    TO  EX-FB-AH-SETTLEMENT-DT.      00007777
LGC003     MOVE CM-LF-DEATH-DT         TO  EX-FB-LF-DEATH-DT.           00007778
LGC003     MOVE CM-ENTRY-DT            TO  EX-FB-ENTRY-DT.              00007779
LGC003*    MOVE SPACES                 TO  EX-FB-AH-EXIT-DT.            00007780
LGC003*    MOVE SPACES                 TO  EX-FB-LF-EXIT-DT.            00007781
LGC003     MOVE CM-LF-CURRENT-STATUS   TO  EX-FB-LF-CURRENT-STATUS.     00007782
LGC003     MOVE SPACES                 TO  EX-FB-LF-ENTRY-STATUS.       00007783
LGC003     MOVE CM-LF-STATUS-AT-DEATH  TO  EX-FB-LF-STATUS-AT-DEATH.    00007784
LGC003     MOVE CM-LF-STATUS-AT-CANCEL TO  EX-FB-LF-STATUS-AT-CANCEL.   00007785
LGC003     MOVE CM-AH-CURRENT-STATUS   TO  EX-FB-AH-CURRENT-STATUS.     00007786
LGC003     MOVE SPACES                 TO  EX-FB-AH-ENTRY-STATUS.       00007787
LGC003     MOVE SPACES                 TO  EX-FB-AH-STATUS-AT-LUMP-SUM. 00007788
LGC003     MOVE CM-AH-STATUS-AT-CANCEL TO  EX-FB-AH-STATUS-AT-CANCEL.   00007789
LGC003     MOVE CM-CLAIM-INTERFACE-SW  TO  EX-FB-CLAIM-INTERFACE-SW.    00007790
LGC003     MOVE CM-CLAIM-ATTACHED-COUNT TO  EX-FB-CLAIM-ATTACHED-COUNT. 00007791
LGC003     MOVE CM-ENTRY-BATCH           TO  EX-FB-ENTRY-BATCH.         00007792
LGC003     MOVE CM-LAST-MONTH-END        TO  EX-FB-LAST-MONTH-END.      00007793
LGC003     MOVE CM-AH-PAID-THRU-DT       TO  EX-FB-AH-PAID-THRU-DT.     00007794
LGC003     MOVE CM-CREDIT-INTERFACE-SW-1 TO  EX-FB-CREDIT-INTERFACE-SW-100007795
LGC003     MOVE CM-CREDIT-INTERFACE-SW-2 TO  EX-FB-CREDIT-INTERFACE-SW-200007796
LGC003     MOVE CM-LIFE-COMM-PCT         TO  EX-FB-LIFE-COMM-PCT.       00007797
LGC003     MOVE CM-AH-COMM-PCT           TO  EX-FB-AH-COMM-PCT.         00007798
LGC003                                                                  00007799
           PERFORM 6000-WRITE-EXTRACT-FILE THRU 6000-EXIT.              00007980
           ADD +1  TO  WS-FB-COUNT.                                     00007990
       4100-EXIT.                                                       00008000
           EXIT.                                                        00008010
                                                                        00008020
           EJECT                                                        00008030
       4200-OUTPUT-FC-RECORD SECTION.                                   00008040
                                                                        00008050
LGC003     MOVE SPACES                 TO  EX-EXTRACT-F-RECORD-C.       00008051
LGC003                                                                  00008052
           MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD       00008060
                                                                        00008070
           MOVE '9'                    TO  EX-POSITIONING-CODE          00008080
           MOVE 'F'                    TO  EX-EXTRACT-CODE              00008090
           MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD                00008100
           MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                00008110
           MOVE 'C'                    TO  EX-RECORD-TYPE               00008120
                                                                        00008130
           MOVE CL-CARRIER             TO  EX-SG-CARRIER                00008140
           MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO             00008150
           MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO               00008160
           MOVE CL-CERT-NO             TO  EX-SG-CERT-NO                00008170
           MOVE AT-SEQUENCE-NO         TO  EX-SG-TRAILER-SEQ-NO         00008180
                                                                        00008190
LGC003     MOVE SPACES                 TO  EX-FC-INSURED-NAME.          00008191
LGC003     MOVE AT-TRAILER-TYPE        TO  EX-FC-TRAILER-TYPE.          00008192
LGC003     MOVE AT-RECORDED-DT         TO  EX-FC-RECORDED-DT            00008193
LGC003     MOVE AT-RECORDED-BY         TO  EX-FC-RECORDED-BY            00008194
LGC003     MOVE AT-LAST-MAINT-HHMMSS   TO  EX-FC-LAST-MAINT-HHMMSS      00008195
LGC003     MOVE AT-TRAILER-BODY        TO  EX-FC-TRAILER-BODY           00008196
           PERFORM 6000-WRITE-EXTRACT-FILE THRU 6000-EXIT.              00008260
           ADD +1  TO  WS-CURRENT-TRLRS                                 00008270
                       WS-FC-COUNT.                                     00008280
       4200-EXIT.                                                       00008290
           EXIT.                                                        00008300
                                                                        00008310
       4300-OUTPUT-REPORT SECTION.                                      00008320
           IF LINES-PRINTED > LINES-ON-PAGE                             00008330
               PERFORM 6100-WRITE-HEADINGS THRU 6100-EXIT.              00008340
                                                                        00008350
           MOVE SPACES                 TO  WS-DETAIL-LINE.              00008360
           MOVE WS-COMPANY-ID          TO  WS-DET-COMPANY.              00008370
           MOVE DA-CARRIER             TO  WS-DET-CARRIER.              00008380
           MOVE DA-CLAIM-NO            TO  WS-DET-CLAIM-NO.             00008390
           MOVE DA-CERT-NO             TO  WS-DET-CERT-NO.              00008400
           MOVE WS-CURRENT-TRLRS       TO  WS-DET-TRLRS.                00008410
           IF DA-RECORD-TYPE = 'A'                                      00008420
               MOVE 'SET UP         '  TO  WS-DET-ACTION.               00008430
           IF DA-RECORD-TYPE = 'C'                                      00008440
               MOVE 'CLOSED         '  TO  WS-DET-ACTION.               00008450
           IF DA-RECORD-TYPE = 'D'                                      00008460
               MOVE 'DENIED         '  TO  WS-DET-ACTION.               00008470
           IF DA-RECORD-TYPE = 'M'                                      00008480
               MOVE 'AUTO PAYMENT   '  TO  WS-DET-ACTION.               00008490
           IF DA-RECORD-TYPE = 'N'                                      00008500
               MOVE 'AUTO CLOSED    '  TO  WS-DET-ACTION.               00008510
           IF DA-RECORD-TYPE = 'O'                                      00008520
               MOVE 'REOPENED       '  TO  WS-DET-ACTION.               00008530
           IF DA-RECORD-TYPE = 'P'                                      00008540
               MOVE 'PAYMENT        '  TO  WS-DET-ACTION.               00008550
           IF DA-RECORD-TYPE = 'V'                                      00008560
               MOVE 'VOID PAYMENT   '  TO  WS-DET-ACTION.               00008570
                                                                        00008580
           IF CLAIM-NOT-FOUND                                           00008590
               MOVE 'CLAIM NOT FOUND'  TO  WS-DET-ACTION.               00008600
                                                                        00008610
           MOVE WS-DETAIL-LINE         TO  PRINT-RECORD.                00008620
           PERFORM 6200-WRITE-DETAIL-LINE THRU 6200-EXIT.               00008630
       4300-EXIT.                                                       00008640
           EXIT.                                                        00008650
                                                                        00008660
LGC003 5000-MOVE-NAME SECTION.   COPY ELCMNS.                           00008661
                                                                        00008680
       6000-WRITE-EXTRACT-FILE SECTION.                                 00008690
           MOVE REPORTS-EXTRACT-RECORD  TO EXTRACT-FILE-RECORD.         00008700
           WRITE EXTRACT-FILE-RECORD.                                   00008710
           ADD +1  TO EXTRACTS-OUT.                                     00008720
       6000-EXIT.                                                       00008730
           EXIT.                                                        00008740
                                                                        00008750
       6100-WRITE-HEADINGS SECTION.                                     00008760
           MOVE WS-HEADING1             TO PRINT-RECORD.                00008770
           WRITE PRINT-RECORD AFTER ADVANCING PAGE.                     00008780
           MOVE WS-RUN-DATE             TO WS-HDG-RUN-DATE.             00008790
           MOVE PAGE-COUNT              TO WS-HDG-PAGE.                 00008800
           MOVE WS-HEADING2             TO PRINT-RECORD.                00008810
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                   00008820
           MOVE WS-HEADING3             TO PRINT-RECORD.                00008830
           WRITE PRINT-RECORD AFTER ADVANCING 2 LINES.                  00008840
           MOVE +5 TO LINES-PRINTED.                                    00008850
           ADD +1  TO PAGE-COUNT.                                       00008860
       6100-EXIT.                                                       00008870
           EXIT.                                                        00008880
                                                                        00008890
       6200-WRITE-DETAIL-LINE SECTION.                                  00008900
           MOVE WS-DETAIL-LINE          TO PRINT-RECORD.                00008910
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINE.                   00008920
           ADD +1  TO LINES-PRINTED.                                    00008930
       6200-EXIT.                                                       00008940
           EXIT.                                                        00008950
                                                                        00008960
           EJECT                                                        00008970
       9000-CLOSE-FILES SECTION.                                        00008980
      *    CLOSE DLYACTV                                                00008990
LGC003     CLOSE ELMSTR                                                 00008991
LGC003           ELCERT                                                 00008992
LGC003           ELTRLR                                                 00008993
LGC003           ERACCT                                                 00008994
LGC003           CIEXTRS                                                00008995
                 PRNTR.                                                 00009050
           MOVE 'ERROR OCCURED CLOSING -'  TO  WS-ERROR-PART1.          00009060
           IF DLYACTV-FILE-STATUS NOT EQUAL ZERO                        00009070
               MOVE 'DLYACTV '         TO  WS-ERROR-PART2               00009080
               MOVE DLYACTV-FILE-STATUS TO WS-ERROR-STATUS              00009090
               PERFORM 9999-ABEND-PGM.                                  00009100
LGC003     IF ELMSTR-FILE-STATUS NOT EQUAL ZERO                         00009101
LGC003         MOVE 'ELMSTR  '         TO  WS-ERROR-PART2               00009102
LGC003         MOVE ELMSTR-FILE-STATUS TO  WS-ERROR-STATUS              00009103
               PERFORM 9999-ABEND-PGM.                                  00009140
LGC003     IF ELCERT-FILE-STATUS NOT EQUAL ZERO                         00009141
LGC003         MOVE 'ELCERT  '         TO  WS-ERROR-PART2               00009142
LGC003         MOVE ELCERT-FILE-STATUS TO  WS-ERROR-STATUS              00009143
               PERFORM 9999-ABEND-PGM.                                  00009180
LGC003     IF ELTRLR-FILE-STATUS NOT EQUAL ZERO                         00009181
LGC003         MOVE 'ELTRLR  '         TO  WS-ERROR-PART2               00009182
LGC003         MOVE ELTRLR-FILE-STATUS TO  WS-ERROR-STATUS              00009183
               PERFORM 9999-ABEND-PGM.                                  00009220
LGC003     IF ERACCT-FILE-STATUS NOT EQUAL ZERO                         00009221
LGC003         MOVE 'ERACCT  '         TO  WS-ERROR-PART2               00009222
LGC003         MOVE ERACCT-FILE-STATUS TO  WS-ERROR-STATUS              00009223
               PERFORM 9999-ABEND-PGM.                                  00009260
           DISPLAY 'TOTAL RECORDS IN   - '  RECORDS-IN.                 00009270
           DISPLAY '  FA EXTRACTS WRITTEN - ' WS-FA-COUNT.              00009280
           DISPLAY '  FB EXTRACTS WRITTEN - ' WS-FB-COUNT.              00009290
           DISPLAY '  FC EXTRACTS WRITTEN - ' WS-FC-COUNT.              00009300
           DISPLAY 'TOTAL RECORDS WRITTEN - ' EXTRACTS-OUT.             00009310
       9000-EXIT.                                                       00009320
           EXIT.                                                        00009330
                                                                        00009340
       9999-ABEND-PGM SECTION.                                          00009350
           DISPLAY WS-ERROR-MESSAGE                                     00009360
           DISPLAY WS-ERROR-MESSAGE UPON CONSOLE                        00009370
           DISPLAY 'PROGRAM WILL NOW ABEND **************'              00009380
           DISPLAY 'PROGRAM WILL NOW ABEND **************'              00009390
                                       UPON CONSOLE.                    00009400
           ADD 1 TO FORCE-DUMP.                                         00009410
       9999-EXIT.                                                       00009420
           EXIT.                                                        00009430
